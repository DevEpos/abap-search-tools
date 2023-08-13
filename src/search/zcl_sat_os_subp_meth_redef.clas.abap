"! <p class="shorttext synchronized">Searches for redefined methods</p>
CLASS zcl_sat_os_subp_meth_redef DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_os_subp_meth_base
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS prepare_search  REDEFINITION.
    METHODS do_after_search REDEFINITION.
    METHODS get_method_key REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_alias_names,
        method TYPE string VALUE 'method',
      END OF c_alias_names,

      BEGIN OF c_fields,
        classname  TYPE string VALUE 'classname',
        package    TYPE string VALUE 'developmentpackage',
        tadir_type TYPE string VALUE 'tadirtype',
      END OF c_fields.

    METHODS add_method_name_filter.
ENDCLASS.


CLASS zcl_sat_os_subp_meth_redef IMPLEMENTATION.
  METHOD prepare_search.
    set_base_select_table( iv_entity = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_classinterface }| )
                           iv_alias  = c_clif_alias ).

    " join to class/interface always necessary because of devclass/tadir type
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>seoredef }|
                    iv_alias      = c_alias_names-method
                    it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                             ( field           = 'clsname'
                                               ref_field       = c_fields-classname
                                               ref_table_alias = c_clif_alias
                                               type            = zif_sat_c_join_cond_type=>field )
                                             ( field           = 'mtdabstrct'
                                               tabname_alias   = c_alias_names-method
                                               type            = zif_sat_c_join_cond_type=>filter
                                               value           = abap_false ) ) ).

    add_select_field( iv_fieldname       = c_fields-classname
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_fields-classname
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = 'mtdname'
                      iv_fieldname_alias = c_result_fields-method_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_fields-tadir_type
                      iv_fieldname_alias = c_result_fields-tadir_type
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname = c_fields-package iv_fieldname_alias = c_result_fields-devclass iv_entity = c_clif_alias ).

    add_order_by( iv_fieldname = c_fields-classname iv_entity = c_clif_alias ).

    configure_class_filters( ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_field_names = VALUE #( ( |{ c_clif_alias }~{ c_fields-classname }| ) ) ).
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

  METHOD get_method_key.
    result = VALUE #( clsname = iv_classname cpdname = iv_method_name ).
  ENDMETHOD.

  METHOD add_method_name_filter.
    DATA lt_temp_method_name_terms TYPE zif_sat_ty_object_search=>ty_s_search_term-values.

    DATA(lr_method_names) = REF #( mo_search_query->mt_search_term[
                                       target = zif_sat_c_object_search=>c_search_fields-method_name_input_key ]-values OPTIONAL ).
    IF lr_method_names IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lr_method_names->* REFERENCE INTO DATA(lr_method_name).
      lt_temp_method_name_terms = VALUE #(
          BASE lt_temp_method_name_terms
          ( sign = lr_method_name->sign option = 'CP' low = |*~{ lr_method_name->low }| ) ).
    ENDLOOP.

    add_search_terms_to_search(
        it_search_terms = VALUE #( BASE lr_method_names->* ( LINES OF lt_temp_method_name_terms ) )
        it_field_names  = VALUE #( ( |{ c_alias_names-method }~mtdname| ) ) ).
  ENDMETHOD.
ENDCLASS.
