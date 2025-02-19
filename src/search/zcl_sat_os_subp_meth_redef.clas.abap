"! <p class="shorttext synchronized">Searches for redefined methods</p>
CLASS zcl_sat_os_subp_meth_redef DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_os_subp_meth_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_method_key_reader~get_method_key REDEFINITION.

  PROTECTED SECTION.
    METHODS prepare_search            REDEFINITION.
    METHODS do_after_search           REDEFINITION.
    METHODS set_method_filters        REDEFINITION.
    METHODS method_matches_filter     REDEFINITION.
    METHODS create_method_info_reader REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_alias_names,
                 method TYPE string VALUE 'method',
               END OF c_alias_names.

    CONSTANTS: BEGIN OF c_fields,
                 classname  TYPE string VALUE 'classname',
                 package    TYPE string VALUE 'developmentpackage',
                 tadir_type TYPE string VALUE 'tadirtype',
                 is_final   TYPE string VALUE 'mtdfinal',
               END OF c_fields.

    DATA mt_changed_on_filter TYPE RANGE OF dats.
    DATA mt_created_on_filter TYPE RANGE OF dats.
    DATA mt_created_by_filter TYPE RANGE OF responsibl.
    DATA mt_changed_by_filter TYPE RANGE OF username.

    METHODS add_method_name_filter.

    METHODS get_date_filter
      IMPORTING
        it_values     TYPE zif_sat_ty_object_search=>ty_t_value_range
      RETURNING
        VALUE(result) LIKE mt_changed_on_filter.

    METHODS configure_method_filters.
ENDCLASS.


CLASS zcl_sat_os_subp_meth_redef IMPLEMENTATION.
  METHOD zif_sat_method_key_reader~get_method_key.
    result = VALUE #( clsname = iv_classname
                      cpdname = iv_method_name ).
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>seoredef
                           iv_alias  = c_alias_names-method ).

    " join to class/interface always necessary because of devclass/tadir type
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterface }|
                    iv_alias      = c_clif_alias
                    it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                             ( field           = c_fields-classname
                                               ref_field       = 'clsname'
                                               ref_table_alias = c_alias_names-method
                                               type            = zif_sat_c_join_cond_type=>field ) ) ).

    add_select_field( iv_fieldname       = c_fields-classname
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_fields-classname
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_clif_alias
                      if_no_grouping     = abap_true ).
    add_select_field( iv_fieldname       = 'mtdname'
                      iv_fieldname_alias = c_result_fields-method_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_fields-tadir_type
                      iv_fieldname_alias = c_result_fields-tadir_type
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_fields-is_final
                      iv_entity          = c_alias_names-method
                      iv_fieldname_alias = c_result_fields-method_is_final ).
    add_select_field( iv_fieldname       = c_fields-package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_clif_alias ).

    add_order_by( iv_fieldname = c_fields-classname
                  iv_entity    = c_clif_alias ).

    configure_class_filters( ).
    add_search_terms_to_search( iv_target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_fields = VALUE #( ( fieldname =  |{ c_clif_alias }~{ c_fields-classname }| ) ) ).

    add_filter( VALUE #( field  = |{ c_alias_names-method }~mtdabstrct|
                         sign   = 'I'
                         option = 'EQ'
                         low    = abap_false ) ).
    configure_method_filters( ).
    add_method_name_filter( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    CHECK mt_result IS NOT INITIAL.

    " get class descriptions
    fill_descriptions( ).

    set_method_filters( ).
    read_method_infos_n_filter( ).

    NEW zcl_sat_meth_subco_filter( ir_result              = REF #( mt_result )
                                   if_use_and_for_options = ms_search_engine_params-use_and_cond_for_options
                                   it_meth_param_filter   = mt_meth_param_filter
                                   it_meth_exc_filter     = mt_meth_exc_filter )->apply( ).
  ENDMETHOD.

  METHOD add_method_name_filter.
    DATA lt_temp_method_name_terms TYPE zif_sat_ty_object_search=>ty_s_search_term-values.

    DATA(lr_method_names) = REF #( mo_search_query->mt_search_term[
                                       target = zif_sat_c_os_meth_options=>c_search_fields-method_name_input_key ]-values OPTIONAL ).
    IF lr_method_names IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lr_method_names->* REFERENCE INTO DATA(lr_method_name).
      lt_temp_method_name_terms = VALUE #(
          BASE lt_temp_method_name_terms
          ( sign = lr_method_name->sign option = 'CP' low = |*~{ lr_method_name->low }| ) ).
    ENDLOOP.

    add_search_terms_to_search( it_search_terms = VALUE #( BASE lr_method_names->*
                                                           ( LINES OF lt_temp_method_name_terms ) )
                                it_fields       = VALUE #( ( fieldname =  |{ c_alias_names-method }~mtdname| ) ) ).
  ENDMETHOD.

  METHOD set_method_filters.
    super->set_method_filters( ).

    LOOP AT mo_search_query->mt_search_options REFERENCE INTO DATA(lr_filter)
         WHERE target = zif_sat_c_os_meth_options=>c_search_fields-method_filter_input_key.

      CASE lr_filter->option.
        WHEN c_general_search_options-changed_by.
          mt_changed_by_filter = CORRESPONDING #( lr_filter->value_range ).
        WHEN c_general_search_options-changed_on.
          mt_changed_on_filter = get_date_filter( lr_filter->value_range ).
        WHEN c_general_search_options-user.
          mt_created_by_filter = CORRESPONDING #( lr_filter->value_range ).
        WHEN c_general_search_options-created_on.
          mt_created_on_filter = get_date_filter( lr_filter->value_range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD method_matches_filter.
    result = super->method_matches_filter( iv_method_name = iv_method_name
                                           is_method      = is_method
                                           is_method_info = is_method_info ).
    IF result = abap_false.
      RETURN.
    ENDIF.

    " Method only matches if admin data filters match
    CLEAR result.

    IF     is_method_info-changedby IN mt_changed_by_filter
       AND is_method_info-changedon IN mt_changed_on_filter
       AND is_method_info-author    IN mt_created_by_filter
       AND is_method_info-createdon IN mt_created_on_filter.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_date_filter.
    TYPES ty_date_range TYPE RANGE OF dats.
    DATA ls_date_range TYPE LINE OF ty_date_range.

    LOOP AT it_values INTO DATA(ls_value).
      ls_date_range = ls_value-low.
      result = VALUE #( BASE result
                        ( ls_date_range ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_method_info_reader.
    result = NEW zcl_sat_method_info_reader( ir_results           = REF #( mt_result )
                                             io_method_key_reader = me
                                             if_only_redefined    = abap_true ).
  ENDMETHOD.

  METHOD configure_method_filters.
    LOOP AT mo_search_query->mt_search_options REFERENCE INTO DATA(lr_filter)
         WHERE target = zif_sat_c_os_meth_options=>c_search_fields-method_filter_input_key.

      IF lr_filter->option = c_method_option-flag.
        LOOP AT lr_filter->value_range INTO DATA(ls_option).
          CASE ls_option-low.
            WHEN zif_sat_c_os_meth_options=>c_method_flags-final.
              add_option_filter(
                  iv_fieldname = |{ c_alias_names-method }~{ c_fields-is_final }|
                  it_values    = VALUE #( ( sign = ls_option-sign option = ls_option-option low = abap_true ) ) ).
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
