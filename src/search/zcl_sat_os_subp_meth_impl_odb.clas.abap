"! <p class="shorttext synchronized">Searches methods by created/changed info of impl. (non-HANA)</p>
"! Provider for searching methods
CLASS zcl_sat_os_subp_meth_impl_odb DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_os_subp_meth_impl.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS prepare_search  REDEFINITION.
    METHODS do_after_search REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_fields,
        classintf  TYPE string VALUE 'classname',
        package    TYPE string VALUE 'developmentpackage',
        tadir_type TYPE string VALUE 'tadirtype',
        progname   TYPE string VALUE 'progname',
        created_by TYPE string VALUE 'cnam',
        created_on TYPE string VALUE 'cdat',
        changed_by TYPE string VALUE 'unam',
        changed_on TYPE string VALUE 'udat',
      END OF c_fields,

      BEGIN OF c_alias_names,
        includes TYPE string VALUE 'incl',
      END OF c_alias_names,

      c_progname_filter_suffix TYPE string VALUE '*CM+++' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_class_info,
        classname          TYPE zsat_i_classinterface-classname,
        developmentpackage TYPE zsat_i_classinterface-developmentpackage,
        tadirtype          TYPE zsat_i_classinterface-tadirtype,
      END OF ty_class_info.

    METHODS configure_incl_filters.
    METHODS read_method_infos_n_filter2.
    METHODS add_progname_search_terms.
    METHODS apply_class_filters.
ENDCLASS.


CLASS zcl_sat_os_subp_meth_impl_odb IMPLEMENTATION.
  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>reposrc
                           iv_alias  = c_alias_names-includes ).

    add_select_field( iv_fieldname       = c_fields-created_by
                      iv_fieldname_alias = c_result_fields-created_by
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_fields-progname
                      iv_fieldname_alias = c_result_fields-method_name
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_fields-created_on
                      iv_fieldname_alias = c_result_fields-created_date
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_fields-changed_by
                      iv_fieldname_alias = c_result_fields-changed_by
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_fields-changed_on
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_alias_names-includes ).

    add_order_by( iv_fieldname = c_fields-progname iv_entity = c_alias_names-includes ).

    configure_incl_filters( ).
    add_progname_search_terms( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    CHECK mt_result IS NOT INITIAL.

    " determine the class name from the found include name
    LOOP AT mt_result REFERENCE INTO DATA(lr_result).
      lr_result->object_name = lr_result->method_name(30).
      TRANSLATE lr_result->object_name(30) USING '= '.
      lr_result->raw_object_name = lr_result->object_name.
    ENDLOOP.

    set_method_filters( ).
    read_method_infos_n_filter2( ).

    apply_class_filters( ).

    NEW zcl_sat_meth_subco_filter( ir_result              = REF #( mt_result )
                                   if_use_and_for_options = ms_search_engine_params-use_and_cond_for_options
                                   it_meth_param_filter   = mt_meth_param_filter
                                   it_meth_exc_filter     = mt_meth_exc_filter )->apply( ).

    " get class descriptions
    fill_descriptions( ).
  ENDMETHOD.

  METHOD configure_incl_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>)
          WHERE target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.

      CASE <ls_option>-option.
        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_fields-created_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_fields-changed_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_fields-created_on }|
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_fields-changed_on }|
                           it_values    = <ls_option>-value_range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD read_method_infos_n_filter2.
    NEW zcl_sat_method_info_reader( ir_results           = REF #( mt_result )
                                    io_method_key_reader = me  )->apply( ).

    LOOP AT mt_result REFERENCE INTO DATA(lr_result).
      IF NOT method_matches_filter( iv_method_name = lr_result->method_decl_method
                                    is_method      = lr_result->*
                                    is_method_info = VALUE #( changedby = lr_result->changed_by
                                                              changedon = lr_result->changed_date
                                                              createdon = lr_result->created_date
                                                              author    = lr_result->created_by ) ).
        DELETE mt_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_progname_search_terms.
    DATA(lt_object_search_terms) = VALUE #( mo_search_query->mt_search_term[
                                                target = zif_sat_c_object_search=>c_search_fields-object_name_input_key ]-values OPTIONAL ).
    IF lt_object_search_terms IS INITIAL.
      " we only want method includes so a manual filter is required
      lt_object_search_terms = VALUE #( ( sign = 'I' option = 'CP' low = c_progname_filter_suffix ) ).
    ELSE.
      LOOP AT lt_object_search_terms REFERENCE INTO DATA(lr_search_term).
        IF lr_search_term->option = 'CP'.
          DATA(lv_last_char_offset) = strlen( lr_search_term->low ) - 1.

          DATA(lv_last_char) = lr_search_term->low+lv_last_char_offset.

          IF lv_last_char = '*'.
            lr_search_term->low = lr_search_term->low && c_progname_filter_suffix.
          ENDIF.
        ELSE.
          " We assume 'EQ' here, and therefore a full classname should have been specified
          lr_search_term->option = 'CP'.
          DATA(lv_progname) = CONV progname( lr_search_term->low ).
          TRANSLATE lv_progname(30) USING ' ='.
          lr_search_term->low = lv_progname && c_progname_filter_suffix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    add_search_terms_to_search( it_search_terms = lt_object_search_terms
                                it_field_names  = VALUE #( ( |{ c_alias_names-includes }~{ c_fields-progname }| ) ) ).
  ENDMETHOD.

  METHOD apply_class_filters.
    DATA lt_class_result TYPE SORTED TABLE OF ty_class_info WITH UNIQUE KEY classname.

    reset( ).

    set_base_select_table( iv_entity = |{ zif_sat_c_select_source_id=>zsat_i_classinterface }| iv_alias = c_clif_alias ).
    configure_class_filters( ).
    new_and_cond_list( ).
    create_where_clause( ).
    create_from_clause( ).

    SELECT classname,
           developmentpackage,
           tadirtype
      FROM (mt_from)
      FOR ALL ENTRIES IN @mt_result
      WHERE (mt_where)
        AND classname = @mt_result-object_name
      INTO CORRESPONDING FIELDS OF TABLE @lt_class_result.

    IF sy-subrc <> 0.
      CLEAR mt_result.
      RETURN.
    ENDIF.

    LOOP AT mt_result REFERENCE INTO DATA(lr_result).
      DATA(lr_class_info) = REF #( lt_class_result[ classname = lr_result->object_name ] OPTIONAL ).
      IF lr_class_info IS NOT BOUND.
        DELETE mt_result.
        CONTINUE.
      ENDIF.

      lr_result->tadir_type = lr_class_info->tadirtype.
      lr_result->devclass   = lr_class_info->developmentpackage.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
