"! <p class="shorttext synchronized">Object search provider for DOMA</p>
CLASS zcl_sat_os_doma_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS prepare_search       REDEFINITION.
    METHODS do_after_search      REDEFINITION.
    METHODS is_grouping_required REDEFINITION.
    METHODS add_having_clauses   REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_base_table TYPE string VALUE 'base'.
    CONSTANTS c_text_table TYPE string VALUE 'text'.
    CONSTANTS c_fixval_table TYPE string VALUE 'fixval'.

    CONSTANTS: BEGIN OF c_fields,
                 tablename           TYPE string VALUE 'tablename',
                 domain_name         TYPE string VALUE 'domainname',
                 language            TYPE string VALUE 'language',
                 data_type           TYPE string VALUE 'datatype',
                 length              TYPE string VALUE 'length',
                 output_length       TYPE string VALUE 'outputlength',
                 decimals            TYPE string VALUE 'decimals',
                 conv_exit           TYPE string VALUE 'convexit',
                 is_lowercase        TYPE string VALUE 'islowercase',
                 has_fix_values      TYPE string VALUE 'hasfixvalues',
                 value_table         TYPE string VALUE 'valuetable',
                 type                TYPE string VALUE 'type',
                 development_package TYPE string VALUE 'developmentpackage',
                 created_by          TYPE string VALUE 'createdby',
                 created_date        TYPE string VALUE 'createddate',
                 changed_by          TYPE string VALUE 'changedby',
                 changed_date        TYPE string VALUE 'changeddate',
                 fix_value_low       TYPE string VALUE 'low',
               END OF c_fields.

    DATA mv_fixval_subquery TYPE string.
    DATA mv_fixval_filter_count TYPE i.

    METHODS configure_filters.

    METHODS add_flag_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_fixval_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS map_flag_opt_to_field
      IMPORTING
        iv_option     TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sat_os_doma_provider IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_fixval_subquery = |SELECT DISTINCT domainname { c_cr_lf }| &&
                         | FROM { zif_sat_c_select_source_id=>zsat_i_domainfixvalue } { c_cr_lf }| &&
                         | WHERE |.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_domain
                           iv_alias  = c_base_table ).

    add_select_field( iv_fieldname       = c_fields-domain_name
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-domain_name
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_base_table
                      if_no_grouping     = abap_true  ).
    add_select_field( iv_fieldname       = c_fields-created_by
                      iv_fieldname_alias = c_result_fields-created_by
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-created_date
                      iv_fieldname_alias = c_result_fields-created_date
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-changed_by
                      iv_fieldname_alias = c_result_fields-changed_by
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-changed_date
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-development_package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = |'{ zif_sat_c_tadir_types=>domain }'|
                      iv_fieldname_alias = c_result_fields-tadir_type
                      if_no_grouping     = abap_true ).

    add_order_by( iv_fieldname = c_fields-domain_name
                  iv_entity    = c_base_table  ).

    add_search_terms_to_search( iv_target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_fields = VALUE #( ( fieldname =  |{ c_base_table }~{ c_fields-domain_name }| ) ) ).

    configure_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD configure_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

        WHEN c_general_search_options-description.
          add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_domaintext }|
                          iv_alias      = c_text_table
                          it_conditions = VALUE #( ( field           = c_fields-domain_name
                                                     ref_field       = c_fields-domain_name
                                                     ref_table_alias = c_base_table
                                                     type            = zif_sat_c_join_cond_type=>field  )
                                                   ( field           = c_fields-language
                                                     tabname_alias   = c_text_table
                                                     value           = sy-langu
                                                     type            = zif_sat_c_join_cond_type=>filter  ) ) ).
          add_option_filter( iv_fieldname = |{ c_text_table }~{ mv_description_filter_field }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = c_fields-created_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = c_fields-created_date
                           it_values    = <ls_option>-value_range ).
        WHEN c_general_search_options-package.
          add_package_filter( iv_fieldname = |{ c_base_table }~{ c_fields-development_package }|
                              it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-software_component.
          add_softw_comp_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-development_package )
                                 iv_ref_table_alias = c_base_table ).

        WHEN c_general_search_options-application_component.
          add_appl_comp_filter( it_values          = <ls_option>-value_range
                                iv_ref_field       = CONV #( c_fields-development_package )
                                iv_ref_table_alias = c_base_table ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = c_fields-changed_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = c_fields-changed_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-release_state.
          add_api_option_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-domain_name )
                                 iv_ref_table_alias = c_base_table
                                 it_tadir_type      = VALUE #( ( zif_sat_c_tadir_types=>domain ) ) ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-length.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-length }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-outlength.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-output_length }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-decimals.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-decimals }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-value_table.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-value_table }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-data_type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-data_type }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_object_search=>c_general_search_params-type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-type }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-conv_exit.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-conv_exit }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-flag.
          add_flag_filter( it_values = <ls_option>-value_range ).

        WHEN zif_sat_c_os_doma_options=>c_filter_key-fix_value.
          add_fixval_filter( it_values = <ls_option>-value_range ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_grouping_required.
    result = super->is_grouping_required( ).
    IF result = abap_true.
      RETURN.
    ENDIF.

    IF mv_fixval_filter_count > 1.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_having_clauses.
    IF mv_fixval_filter_count > 1.
      add_having_clause( iv_field           = |{ c_fixval_table }~{ c_fields-fix_value_low }|
                         iv_counter_compare = mv_fixval_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD do_after_search.
    fill_descriptions( ).
  ENDMETHOD.

  METHOD add_flag_filter.
    DATA ls_value TYPE zif_sat_ty_object_search=>ty_s_value_range.

    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    LOOP AT lt_excluding INTO ls_value.
      add_option_filter( iv_fieldname = map_flag_opt_to_field( ls_value-low )
                         it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = abap_false ) ) ).
    ENDLOOP.

    IF lt_including IS INITIAL.
      RETURN.
    ENDIF.

    IF ms_search_engine_params-use_and_cond_for_options = abap_true.
      LOOP AT lt_including INTO ls_value.
        add_option_filter( iv_fieldname = map_flag_opt_to_field( ls_value-low )
                           it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ) ).
      ENDLOOP.
    ELSE.
      new_and_cond_list( ).

      LOOP AT lt_including INTO ls_value.
        add_option_filter( iv_fieldname = map_flag_opt_to_field( ls_value-low )
                           it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ) ).
        new_or_cond_list( ).
      ENDLOOP.

      new_and_cond_list( ).
    ENDIF.
  ENDMETHOD.

  METHOD map_flag_opt_to_field.
    result = SWITCH string( iv_option
                            WHEN zif_sat_c_os_doma_options=>c_flag-lowercase THEN
                              |{ c_base_table }~{ c_fields-is_lowercase }|
                            WHEN zif_sat_c_os_doma_options=>c_flag-fix_values THEN
                              |{ c_base_table }~{ c_fields-has_fix_values }| ).
  ENDMETHOD.

  METHOD add_fixval_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-fix_value_low
                            iv_fieldname          = |{ c_base_table }~{ c_fields-domain_name }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_fixval_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      set_distinct_required( ).
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_domainfixvalue }|
                      iv_alias      = |{ c_fixval_table }|
                      it_conditions = VALUE #( ( field           = c_fields-domain_name
                                                 ref_field       = c_fields-domain_name
                                                 ref_table_alias = c_base_table
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).

      add_option_filter( iv_fieldname = |{ c_fixval_table }~{ c_fields-fix_value_low }|
                         it_values    = lt_including ).

      mv_fixval_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
