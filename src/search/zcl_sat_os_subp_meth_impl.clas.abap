"! <p class="shorttext synchronized">Searches methods by created/changed info of impl.</p>
"! Provider for searching methods
CLASS zcl_sat_os_subp_meth_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zcl_sat_os_classintf_provider.

  PUBLIC SECTION.
    METHODS is_search_possible
      IMPORTING
        io_query      TYPE REF TO zif_sat_object_search_query
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
    METHODS prepare_search  REDEFINITION.
    METHODS do_after_search REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.
    ALIASES c_method_option            FOR zif_sat_c_object_search~c_method_search_option.

    CONSTANTS:
      BEGIN OF c_fields,
        classintf  TYPE string VALUE 'classname',
        package    TYPE string VALUE 'developmentpackage',
        tadir_type TYPE string VALUE 'tadirtype',
      END OF c_fields,

      BEGIN OF c_alias_names,
        includes TYPE string VALUE 'incl',
      END OF c_alias_names.

    TYPES:
      BEGIN OF ty_method,
        clsname      TYPE seoclsname,
        parent_class TYPE seoclsname,
        cmpname      TYPE seocmpname,
        exposure     TYPE seoexpose,
        final        TYPE abap_bool,
        created_by   TYPE uname,
        created_on   TYPE dats,
        changed_by   TYPE uname,
        changed_on   TYPE dats,
      END OF ty_method,

      BEGIN OF ty_class,
        classname          TYPE zsat_i_classinterface-classname,
        developmentpackage TYPE zsat_i_classinterface-developmentpackage,
        tadirtype          TYPE zsat_i_classinterface-tadirtype,
      END OF ty_class,

      ty_user_range TYPE RANGE OF author,
      ty_date_range TYPE RANGE OF dats,

      BEGIN OF ty_include,
        progname TYPE reposrc-progname,
      END OF ty_include.

    DATA mt_meth_final_filter    TYPE RANGE OF abap_bool.
    DATA mt_meth_cls_exc_filter  TYPE RANGE OF abap_bool.
    DATA mt_meth_level_filter    TYPE RANGE OF seomtddecl.
    DATA mt_meth_type_filter     TYPE RANGE OF seomtdtype.
    DATA mt_meth_status_filter   TYPE RANGE OF string.
    DATA mt_meth_exposure_filter TYPE RANGE OF seoexpose.
    DATA mt_meth_name_filter     TYPE RANGE OF string.
    DATA mt_meth_param_filter    TYPE RANGE OF seosconame.
    DATA mt_meth_exc_filter      TYPE RANGE OF seosconame.

    METHODS read_method_infos_n_filter.

    METHODS set_method_filters.
    METHODS configure_incl_filters.

    METHODS method_matches_filter
      IMPORTING
        is_method      TYPE zif_sat_ty_object_search=>ty_s_search_result
        is_method_info TYPE vseomethod
      RETURNING
        VALUE(result)  TYPE abap_bool.

    METHODS add_admin_data_filters.

    METHODS select_includes.
ENDCLASS.


CLASS zcl_sat_os_subp_meth_impl IMPLEMENTATION.
  METHOD is_search_possible.
    LOOP AT io_query->mt_search_options REFERENCE INTO DATA(lr_option) ##NEEDED
          WHERE     target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                AND (    option = zif_sat_c_object_search=>c_general_search_params-changed_by
                      OR option = zif_sat_c_object_search=>c_general_search_params-user
                      OR option = zif_sat_c_object_search=>c_general_search_params-changed_on
                      OR option = zif_sat_c_object_search=>c_general_search_params-created_on ).

      result = abap_true.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_classinterface
                           iv_alias  = c_clif_alias ).

    IF sy-dbsys = 'HDB'.
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_reposource }|
                      iv_alias      = c_alias_names-includes
                      it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                               ( field           = 'objectname'
                                                 ref_field       = c_fields-classintf
                                                 ref_table_alias = c_clif_alias
                                                 type            = zif_sat_c_join_cond_type=>field )
                                               ( field           = 'maintype'
                                                 tabname_alias   = c_alias_names-includes
                                                 type            = zif_sat_c_join_cond_type=>filter
                                                 value           = 'C' )
                                               ( field           = 'includekind'
                                                 tabname_alias   = c_alias_names-includes
                                                 type            = zif_sat_c_join_cond_type=>filter
                                                 value           = 'M*'
                                                 operator        = zif_sat_c_operator=>like ) ) ).
    ENDIF.

    add_select_field( iv_fieldname       = c_fields-classintf
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_fields-classintf
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname = c_fields-package iv_fieldname_alias = c_result_fields-devclass iv_entity = c_clif_alias ).
    add_select_field( iv_fieldname       = c_fields-tadir_type
                      iv_fieldname_alias = c_result_fields-tadir_type
                      iv_entity          = c_clif_alias ).

    IF sy-dbsys = 'HDB'.
      add_select_field( iv_fieldname       = c_general_fields-created_by
                        iv_fieldname_alias = c_result_fields-created_by
                        iv_entity          = c_alias_names-includes ).
      add_select_field( iv_fieldname       = 'progname'
                        iv_fieldname_alias = c_result_fields-method_name
                        iv_entity          = c_alias_names-includes ).
      add_select_field( iv_fieldname       = c_general_fields-created_on
                        iv_fieldname_alias = c_result_fields-created_date
                        iv_entity          = c_alias_names-includes ).
      add_select_field( iv_fieldname       = c_general_fields-changed_by
                        iv_fieldname_alias = c_result_fields-changed_by
                        iv_entity          = c_alias_names-includes ).
      add_select_field( iv_fieldname       = c_general_fields-changed_on
                        iv_fieldname_alias = c_result_fields-changed_date
                        iv_entity          = c_alias_names-includes ).
    ENDIF.

    add_order_by( iv_fieldname = c_fields-classintf iv_entity = c_clif_alias ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_field_names = VALUE #( ( |{ c_clif_alias }~{ c_fields-classintf }| ) ) ).
    configure_class_filters( ).

    IF sy-dbsys = 'HDB'.
      configure_incl_filters( ).
    ENDIF.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    CHECK mt_result IS NOT INITIAL.

    " get class descriptions
    fill_descriptions( ).

    if sy-dbsys <> 'HDB'.
      select_includes( ).
    ENDIF.

    set_method_filters( ).
    read_method_infos_n_filter( ).

    NEW zcl_sat_meth_subco_filter( ir_result              = REF #( mt_result )
                                   if_use_and_for_options = ms_search_engine_params-use_and_cond_for_options
                                   it_meth_param_filter   = mt_meth_param_filter
                                   it_meth_exc_filter     = mt_meth_exc_filter )->apply( ).
  ENDMETHOD.

  METHOD configure_incl_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>)
          WHERE target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.

      CASE <ls_option>-option.
        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_general_fields-created_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_general_fields-changed_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_general_fields-created_on }|
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = |{ c_alias_names-includes }~{ c_general_fields-changed_on }|
                           it_values    = <ls_option>-value_range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD read_method_infos_n_filter.
    DATA ls_method_info    TYPE vseomethod.
    DATA ls_method_details TYPE seoo_method_details.

    LOOP AT mt_result REFERENCE INTO DATA(lr_class_method).
      cl_oo_classname_service=>get_method_by_include(
        EXPORTING  incname             = CONV #( lr_class_method->method_name )
        RECEIVING  mtdkey              = DATA(ls_mtdkey)
        EXCEPTIONS class_not_existing  = 1
                   method_not_existing = 2
                   OTHERS              = 3 ).
      IF sy-subrc <> 0.
        DELETE mt_result.
        CONTINUE.
      ENDIF.

      CLEAR: ls_method_info,
             ls_method_details.

      CALL FUNCTION 'SEO_METHOD_GET_DETAIL'
        EXPORTING  cpdkey         = ls_mtdkey
        IMPORTING  method         = ls_method_info
                   method_details = ls_method_details
        EXCEPTIONS not_existing   = 1
                   no_method      = 2
                   OTHERS         = 3.

      IF sy-subrc <> 0.
        DELETE mt_result.
        CONTINUE.
      ENDIF.

      lr_class_method->method_descr       = ls_method_info-descript.
      lr_class_method->method_exposure    = ls_method_info-exposure.
      lr_class_method->method_name        = ls_mtdkey-cpdname.
      lr_class_method->method_decl_clif   = ls_method_info-clsname.
      lr_class_method->method_decl_method = ls_method_info-cmpname.
      lr_class_method->method_is_final    = ls_method_info-mtdfinal.
      lr_class_method->method_type        = ls_method_info-mtdtype.
      lr_class_method->method_level       = ls_method_info-mtddecltyp.
      lr_class_method->method_status      = COND #(
        WHEN ls_method_details-is_abstract_implemented  = abap_true
          OR ls_method_details-is_final_implemented     = abap_true
          OR ls_method_details-is_implemented_if_method = abap_true THEN
          zif_sat_c_object_search=>c_method_status_int-implemented
        WHEN ls_method_details-is_final_redefined          = abap_true
          OR ls_method_details-is_redefined                = abap_true
          OR ls_method_details-is_final_redefined_in_super = abap_true THEN
          zif_sat_c_object_search=>c_method_status_int-redefined
        ELSE
          zif_sat_c_object_search=>c_method_status_int-standard ).

      IF NOT method_matches_filter( is_method = lr_class_method->* is_method_info = ls_method_info ).
        DELETE mt_result.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_method_filters.
    LOOP AT mo_search_query->mt_search_options REFERENCE INTO DATA(lr_filter)
          WHERE target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.

      CASE lr_filter->option.

        WHEN c_method_option-level.
          mt_meth_level_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-visibility.
          mt_meth_exposure_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_general_search_options-type.
          mt_meth_type_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-status.
          mt_meth_status_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-exception.
          mt_meth_exc_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-param.
          mt_meth_param_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-flag.
          LOOP AT lr_filter->value_range INTO DATA(ls_option).
            CASE ls_option-low.
              WHEN zif_sat_c_object_search=>c_method_flags-final.
                mt_meth_final_filter = VALUE #( ( sign = ls_option-sign option = ls_option-option low = abap_true ) ).
              WHEN zif_sat_c_object_search=>c_method_flags-class_exceptions.
                mt_meth_cls_exc_filter = VALUE #( ( sign = ls_option-sign option = ls_option-option low = abap_true ) ).

            ENDCASE.
          ENDLOOP.

      ENDCASE.
    ENDLOOP.

    mt_meth_name_filter = VALUE #( mo_search_query->mt_search_term[
                                       target = zif_sat_c_object_search=>c_search_fields-method_name_input_key ]-values OPTIONAL ).
  ENDMETHOD.

  METHOD method_matches_filter.
    IF     is_method-method_exposure IN mt_meth_exposure_filter
       AND is_method-method_level    IN mt_meth_level_filter
       AND is_method-method_is_final IN mt_meth_final_filter
       AND is_method-method_name     IN mt_meth_name_filter
       AND is_method-method_type     IN mt_meth_type_filter
       AND is_method-method_status   IN mt_meth_status_filter
       AND is_method_info-mtdnewexc  IN mt_meth_cls_exc_filter.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD select_includes.
    TYPES:
      BEGIN OF ty_method_include,
        progname TYPE progname,
        clsname  TYPE classname,
      END OF ty_method_include.

    DATA lt_progname_filter TYPE TABLE OF progname.
    DATA lt_method_includes TYPE SORTED TABLE OF ty_method_include WITH NON-UNIQUE KEY clsname.

    LOOP AT mt_result REFERENCE INTO DATA(lr_cls_result).
      DATA(lt_all_method_includes) = cl_oo_classname_service=>get_all_method_includes(
                                         clsname = lr_cls_result->object_name ).
      lt_progname_filter = VALUE #( BASE lt_progname_filter
                                    FOR incl IN lt_all_method_includes
                                    ( incl-incname ) ).
    ENDLOOP.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: mt_where,
           mt_criteria,
           mt_criteria_and,
           mt_criteria_or.

    add_admin_data_filters( ).
    new_and_cond_list( ).
    create_where_clause( ).

    DATA(lv_max_rows) = mo_search_query->mv_max_rows + 1.

    " find method includes by class filter
    SELECT progname
      FROM reposrc
      FOR ALL ENTRIES IN @lt_progname_filter
      WHERE (mt_where)
        AND progname = @lt_progname_filter-table_line
      INTO CORRESPONDING FIELDS OF TABLE @lt_method_includes
      UP TO @lv_max_rows ROWS.

    IF sy-subrc <> 0.
      CLEAR mt_result.
      RETURN.
    ENDIF.

    " extract class name from include name
    DATA(lt_incl_temp) = lt_method_includes.
    CLEAR lt_method_includes.
    LOOP AT lt_incl_temp REFERENCE INTO DATA(lr_meth_include).
      INSERT VALUE #( progname = lr_meth_include->progname
                      clsname  = translate( val  = lr_meth_include->progname(30)
                                            from = `=`
                                            to   = ` ` ) ) INTO TABLE lt_method_includes.
    ENDLOOP.

    DATA(lt_result_tmp) = mt_result.
    CLEAR mt_result.
    LOOP AT lt_result_tmp REFERENCE INTO lr_cls_result.
      LOOP AT lt_method_includes REFERENCE INTO lr_meth_include WHERE clsname = lr_cls_result->object_name.
        lr_cls_result->method_name = lr_meth_include->progname.
        mt_result = VALUE #( BASE mt_result ( lr_cls_result->* ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_admin_data_filters.
    LOOP AT mo_search_query->mt_search_options REFERENCE INTO DATA(lr_filter)
            WHERE target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.

      CASE lr_filter->option.
        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = 'unam'
                             it_values    = lr_filter->value_range ).
        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = 'udat'
                           it_values    = lr_filter->value_range ).
        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = 'cnam'
                             it_values    = lr_filter->value_range ).
        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = 'cdat'
                           it_values    = lr_filter->value_range ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
