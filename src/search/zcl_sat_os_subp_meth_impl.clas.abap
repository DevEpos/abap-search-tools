"! <p class="shorttext synchronized">Searches methods by created/changed info of impl. (HANA)</p>
"! Provider for searching methods
CLASS zcl_sat_os_subp_meth_impl DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_os_subp_meth_base.

  PUBLIC SECTION.
    METHODS is_search_possible
      IMPORTING
        io_query      TYPE REF TO zif_sat_object_search_query
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS zif_sat_method_key_reader~get_method_key REDEFINITION.

  PROTECTED SECTION.
    METHODS prepare_search  REDEFINITION.
    METHODS do_after_search REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_fields,
        classintf  TYPE string VALUE 'classname',
        package    TYPE string VALUE 'developmentpackage',
        tadir_type TYPE string VALUE 'tadirtype',
      END OF c_fields,

      BEGIN OF c_alias_names,
        includes TYPE string VALUE 'incl',
      END OF c_alias_names.

    METHODS configure_incl_filters.
    METHODS read_method_infos_n_filter2.
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

  METHOD zif_sat_method_key_reader~get_method_key.
    cl_oo_classname_service=>get_method_by_include( EXPORTING  incname             = CONV #( iv_method_name )
                                                    RECEIVING  mtdkey              = result
                                                    EXCEPTIONS class_not_existing  = 1
                                                               method_not_existing = 2
                                                               OTHERS              = 3 ).
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_classinterface
                           iv_alias  = c_clif_alias ).

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

    add_select_field( iv_fieldname       = c_fields-classintf
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_fields-classintf
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_general_fields-created_by
                      iv_fieldname_alias = c_result_fields-created_by
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname = c_fields-package iv_fieldname_alias = c_result_fields-devclass iv_entity = c_clif_alias ).
    add_select_field( iv_fieldname       = 'progname'
                      iv_fieldname_alias = c_result_fields-method_name
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_fields-tadir_type
                      iv_fieldname_alias = c_result_fields-tadir_type
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_general_fields-created_on
                      iv_fieldname_alias = c_result_fields-created_date
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_general_fields-changed_by
                      iv_fieldname_alias = c_result_fields-changed_by
                      iv_entity          = c_alias_names-includes ).
    add_select_field( iv_fieldname       = c_general_fields-changed_on
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_alias_names-includes ).

    add_order_by( iv_fieldname = c_fields-classintf iv_entity = c_clif_alias ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_field_names = VALUE #( ( |{ c_clif_alias }~{ c_fields-classintf }| ) ) ).
    set_type_filter_to_class( ).
    configure_class_filters( ).
    configure_incl_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    CHECK mt_result IS NOT INITIAL.

    " get class descriptions
    fill_descriptions( ).

    set_method_filters( ).
*    read_method_infos_n_filter( ).
    read_method_infos_n_filter2( ).

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
ENDCLASS.
