"! <p class="shorttext synchronized">Database table/view search provider for Object search</p>
CLASS zcl_sat_os_dbtab_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_os_dtab_options.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor.

  PROTECTED SECTION.
    METHODS prepare_search       REDEFINITION.
    METHODS is_grouping_required REDEFINITION.
    METHODS add_having_clauses   REDEFINITION.
    METHODS do_after_search      REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_dbtab_search_params FOR zif_sat_c_os_dtab_options~c_filter_key.

    CONSTANTS c_base_table TYPE string VALUE 'base'.
    CONSTANTS c_field_table TYPE string VALUE 'field'.
    CONSTANTS c_include_table TYPE string VALUE 'incl_usage'.
    CONSTANTS c_tech_settings_table TYPE string VALUE 'tech'.
    CONSTANTS: BEGIN OF c_fields,
                 alias                      TYPE string VALUE 'field',
                 tablename                  TYPE string VALUE 'tablename',
                 entityid                   TYPE string VALUE 'entityid',
                 type                       TYPE string VALUE 'type',
                 fieldname                  TYPE string VALUE 'fieldname',
                 include_name               TYPE string VALUE 'includename',
                 description                TYPE string VALUE 'description',
                 development_package        TYPE string VALUE 'developmentpackage',
                 delivery_class             TYPE string VALUE 'deliveryclass',
                 created_by                 TYPE string VALUE 'createdby',
                 created_date               TYPE string VALUE 'createddate',
                 changed_by                 TYPE string VALUE 'changedby',
                 changed_date               TYPE string VALUE 'changeddate',
                 extension_class            TYPE string VALUE 'extensionclass',
                 maintenance_flag           TYPE string VALUE 'maintenanceflag',
                 search_help_binding_exists TYPE string VALUE 'searchhelpbindingexists',
                 client_dependent           TYPE string VALUE 'clientdependent',
                 is_gtt                     TYPE string VALUE 'isgtt',
               END OF c_fields.

    CONSTANTS: BEGIN OF c_tech_fields,
                 change_log_active TYPE string VALUE 'protokoll',
                 buffering_status  TYPE string VALUE 'bufallow',
                 buffering_type    TYPE string VALUE 'pufferung',
                 size_category     TYPE string VALUE 'tabkat',
                 data_class        TYPE string VALUE 'tabart',
                 storage_type      TYPE string VALUE 'roworcolst',
               END OF c_tech_fields.

    DATA mv_field_subquery TYPE string.
    DATA mv_field_filter_count TYPE i.
    DATA mv_incl_filter_count TYPE i.
    DATA mf_dd09l_join_needed TYPE abap_bool.

    "! <p class="shorttext synchronized">Create filter for FIELD option</p>
    METHODS add_field_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_include_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS configure_filters.

    METHODS add_flag_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS map_flag_opt_to_field
      IMPORTING
        iv_option     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS add_tech_tab_join.
ENDCLASS.


CLASS zcl_sat_os_dbtab_provider IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_field_subquery = |SELECT DISTINCT tablename { c_cr_lf }| &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_tablefield } { c_cr_lf }| &&
                        | WHERE |.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_databasetable
                           iv_alias  = c_base_table ).

    add_select_field( iv_fieldname       = c_fields-tablename
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-tablename
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
    add_select_field( iv_fieldname       = c_fields-type
                      iv_fieldname_alias = c_result_fields-entity_type
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = |'{ zif_sat_c_tadir_types=>table }'|
                      iv_fieldname_alias = c_result_fields-tadir_type
                      if_no_grouping     = abap_true ).

    add_order_by( iv_fieldname = c_fields-tablename
                  iv_entity    = c_base_table  ).

    add_search_terms_to_search( iv_target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_fields = VALUE #( ( fieldname =  |{ c_base_table }~{ c_fields-tablename }| ) ) ).

    configure_filters( ).

    IF mf_dd09l_join_needed = abap_true.
      add_tech_tab_join( ).
    ENDIF.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    fill_descriptions( ).
  ENDMETHOD.

  METHOD is_grouping_required.
    result = super->is_grouping_required( ).
    IF result = abap_true.
      RETURN.
    ENDIF.

    IF    mv_field_filter_count > 1
       OR mv_incl_filter_count  > 1.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_having_clauses.
    IF mv_field_filter_count > 1.
      add_having_clause( iv_field           = |{ c_field_table }~{ c_fields-fieldname }|
                         iv_counter_compare = mv_field_filter_count ).
    ENDIF.

    IF mv_incl_filter_count > 1.
      add_having_clause( iv_field           = |{ c_include_table }~{ c_fields-include_name }|
                         iv_counter_compare = mv_incl_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD configure_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

        WHEN c_general_search_options-description.
          add_option_filter( iv_fieldname = mv_description_filter_field
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

        WHEN zif_sat_c_os_tabl_options=>c_filter_key-field.
          add_field_filter( <ls_option>-value_range ).

        WHEN c_dbtab_search_params-delivery_class.
          add_option_filter( iv_fieldname = c_fields-delivery_class
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = c_fields-changed_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = c_fields-changed_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_dbtab_search_params-flag.
          add_flag_filter( <ls_option>-value_range ).

        WHEN c_general_search_options-release_state.
          add_api_option_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-tablename )
                                 iv_ref_table_alias = c_base_table
                                 it_tadir_type      = VALUE #( ( zif_sat_c_tadir_types=>table ) ) ).

        WHEN zif_sat_c_os_tabl_options=>c_filter_key-enhancement_category.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-extension_class }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_dbtab_search_params-buffering.
          mf_dd09l_join_needed = abap_true.
          add_option_filter( iv_fieldname = |{ c_tech_settings_table }~{ c_tech_fields-buffering_status }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_dbtab_search_params-data_class.
          mf_dd09l_join_needed = abap_true.
          add_option_filter( iv_fieldname = |{ c_tech_settings_table }~{ c_tech_fields-data_class }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_dbtab_search_params-size_category.
          mf_dd09l_join_needed = abap_true.
          add_option_filter( iv_fieldname = |{ c_tech_settings_table }~{ c_tech_fields-size_category }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_dbtab_search_params-buffering_type.
          mf_dd09l_join_needed = abap_true.
          add_option_filter( iv_fieldname = |{ c_tech_settings_table }~{ c_tech_fields-buffering_type }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_dbtab_search_params-storage_type.
          mf_dd09l_join_needed = abap_true.
          add_option_filter( iv_fieldname = |{ c_tech_settings_table }~{ c_tech_fields-storage_type }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-maintenance.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-maintenance_flag }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_tabl_options=>c_filter_key-include_usage.
          add_include_filter( it_values = <ls_option>-value_range  ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_field_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-fieldname
                            iv_fieldname          = |{ c_base_table }~{ c_fields-tablename }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_field_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      set_distinct_required( ).
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tablefield }|
                      iv_alias      = |{ c_field_table }|
                      it_conditions = VALUE #( ( field           = c_fields-tablename
                                                 ref_field       = c_fields-tablename
                                                 ref_table_alias = c_base_table
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).

      add_option_filter( iv_fieldname = |{ c_field_table }~{ c_fields-fieldname }|
                         it_values    = lt_including ).

      mv_field_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_include_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-include_name
                            iv_fieldname          = |{ c_base_table }~{ c_fields-tablename }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_field_subquery ).
    ENDIF.

    IF lt_including IS INITIAL.
      RETURN.
    ENDIF.

    set_distinct_required( ).
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tablefield }|
                    iv_alias      = |{ c_include_table }|
                    it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                             ( field           = c_fields-tablename
                                               ref_field       = c_fields-tablename
                                               ref_table_alias = c_base_table
                                               type            = zif_sat_c_join_cond_type=>field )
                                             ( field           = c_fields-fieldname
                                               tabname_alias   = c_include_table
                                               value           = '.INCLUDE'
                                               type            = zif_sat_c_join_cond_type=>filter ) ) ).

    add_option_filter( iv_fieldname = |{ c_include_table }~{ c_fields-include_name }|
                       it_values    = lt_including ).

    mv_incl_filter_count = lines( lt_including ).
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

    IF line_exists( it_values[ low = zif_sat_c_os_dtab_options=>c_db_flags-change_log_active ] ).
      mf_dd09l_join_needed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD map_flag_opt_to_field.
    result = SWITCH string( iv_option
                            WHEN zif_sat_c_os_dtab_options=>c_db_flags-client_dep THEN
                              |{ c_base_table }~{ c_fields-client_dependent }|
                            WHEN zif_sat_c_os_dtab_options=>c_db_flags-used_in_shlp THEN
                              |{ c_base_table }~{ c_fields-search_help_binding_exists }|
                            WHEN zif_sat_c_os_dtab_options=>c_db_flags-is_gtt THEN
                              |{ c_base_table }~{ c_fields-is_gtt }|
                            WHEN zif_sat_c_os_dtab_options=>c_db_flags-change_log_active THEN
                              |{ c_tech_settings_table }~{ c_tech_fields-change_log_active }| ).
  ENDMETHOD.

  METHOD add_tech_tab_join.
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>dd09l }|
                    iv_alias      = c_tech_settings_table
                    iv_join_type  = zif_sat_c_join_types=>inner_join
                    it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                             ( field           = 'tabname'
                                               ref_field       = c_fields-tablename
                                               ref_table_alias = c_base_table
                                               type            = zif_sat_c_join_cond_type=>field )
                                             ( field           = 'as4local'
                                               tabname_alias   = c_tech_settings_table
                                               type            = zif_sat_c_join_cond_type=>filter
                                               value           = 'A' ) ) ).
  ENDMETHOD.
ENDCLASS.
