"! <p class="shorttext synchronized">Search provider for Dictionary Views</p>
CLASS zcl_sat_os_ddicview_provider DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_base_search_provider.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS prepare_search     REDEFINITION.
    METHODS determine_grouping REDEFINITION.
    methods do_after_search REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_search_params FOR zif_sat_c_object_search~c_ddicview_search_params.

    CONSTANTS:
      c_base_table     TYPE string VALUE 'base',
      c_field_table    TYPE string VALUE 'field',
      c_base_tab_table TYPE string VALUE 'basetab',

      BEGIN OF c_fields,
        alias               TYPE string VALUE 'field',
        viewname            TYPE string VALUE 'viewname',
        tablename           TYPE string VALUE 'tablename',
        type                TYPE string VALUE 'viewclass',
        fieldname           TYPE string VALUE 'fieldname',
        description         TYPE string VALUE 'description',
        primary_table       TYPE string VALUE 'primarytable',
        development_package TYPE string VALUE 'developmentpackage',
        delivery_class      TYPE string VALUE 'deliveryclass',
        created_by          TYPE string VALUE 'createdby',
        created_date        TYPE string VALUE 'createddate',
        changed_by          TYPE string VALUE 'changedby',
        changed_date        TYPE string VALUE 'changeddate',
        maintenance_flag    TYPE string VALUE 'maintenanceflag',
      END OF c_fields.

    DATA mv_field_subquery       TYPE string.
    DATA mv_basetab_subquery     TYPE string.

    DATA mv_field_filter_count   TYPE i.
    DATA mv_basetab_filter_count TYPE i.

    METHODS configure_filters.

    METHODS add_field_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_basetab_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.
ENDCLASS.


CLASS zcl_sat_os_ddicview_provider IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_field_subquery = |SELECT DISTINCT tablename | && c_cr_lf &&
                        | FROM { get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_tablefield }| ) } | && c_cr_lf &&
                        | WHERE |.
    mv_basetab_subquery = |SELECT DISTINCT viewname | && c_cr_lf &&
                          | FROM { get_cds_sql_name( |{ zif_sat_c_select_source_id=>dd26s }| ) } | && c_cr_lf &&
                          | WHERE |.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table(
        iv_entity     = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_ddicview }| )
        iv_alias      = c_base_table
        it_parameters = VALUE #(
            ( param_name = 'p_language' param_value = zcl_sat_system_helper=>get_system_language( ) ) ) ).

    add_select_field( iv_fieldname = c_fields-viewname iv_fieldname_alias = c_result_fields-object_name iv_entity = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-viewname
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname = c_fields-created_by iv_fieldname_alias = c_result_fields-created_by iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-created_date iv_fieldname_alias = c_result_fields-created_date iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-changed_by iv_fieldname_alias = c_result_fields-changed_by iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-changed_date iv_fieldname_alias = c_result_fields-changed_date iv_entity = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-development_package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = |'{ zif_sat_c_entity_type=>view }'|
                      iv_fieldname_alias = c_result_fields-entity_type ).
    add_select_field( iv_fieldname       = |'{ zif_sat_c_tadir_types=>view }'| iv_fieldname_alias = c_result_fields-tadir_type ).

    add_order_by( iv_fieldname = c_fields-viewname iv_entity = c_base_table  ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_field_names = VALUE #( ( |{ c_base_table }~{ c_fields-viewname }| ) ) ).

    configure_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    fill_descriptions( ).
  ENDMETHOD.

  METHOD determine_grouping.
    CHECK ms_search_engine_params-use_and_cond_for_options = abap_true.

    IF NOT ( mv_field_filter_count > 1 OR mv_basetab_filter_count > 1 ).
      RETURN.
    ENDIF.

    add_group_by_clause( |{ c_base_table }~{ c_fields-viewname }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-viewname }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-created_by }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-created_date }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-changed_by }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-changed_date }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-development_package }| ).
    add_group_by_clause( |{ c_base_table }~{ c_fields-type }| ).

    IF mv_field_filter_count > 1.
      add_having_clause( iv_field = |{ c_field_table }~{ c_fields-fieldname }| iv_counter_compare = mv_field_filter_count ).
    ENDIF.
    IF mv_basetab_filter_count > 1.
      add_having_clause( iv_field = |{ c_base_tab_table }~tabname| iv_counter_compare = mv_basetab_filter_count ).
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
          add_package_filter( iv_fieldname = c_fields-development_package
                              it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-software_component.
          add_softw_comp_filter( if_use_ddic_sql_view = abap_true
                                 it_values            = <ls_option>-value_range
                                 iv_ref_field         = CONV #( c_fields-development_package )
                                 iv_ref_table_alias   = c_base_table ).

        WHEN c_general_search_options-application_component.
          add_appl_comp_filter( if_use_ddic_sql_view = abap_true
                                it_values            = <ls_option>-value_range
                                iv_ref_field         = CONV #( c_fields-development_package )
                                iv_ref_table_alias   = c_base_table ).

        WHEN c_general_search_options-type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-type }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_search_params-field.
          add_field_filter( <ls_option>-value_range ).

        WHEN c_search_params-base_table.
          add_basetab_filter( <ls_option>-value_range ).

        WHEN c_search_params-primary_table.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-primary_table }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_search_params-delivery_class.
          add_option_filter( iv_fieldname = c_fields-delivery_class
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = c_fields-changed_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = c_fields-changed_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-maintenance.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-maintenance_flag }|
                             it_values    = <ls_option>-value_range ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_field_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-fieldname
                            iv_fieldname          = |{ c_base_table }~{ c_fields-viewname }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_field_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table( iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_tablefield }| )
                      iv_alias      = c_field_table
                      it_conditions = VALUE #( ( field           = c_fields-tablename
                                                 ref_field       = c_fields-viewname
                                                 ref_table_alias = c_base_table
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).

      add_option_filter( iv_fieldname = |{ c_field_table }~{ c_fields-fieldname }|
                         it_values    = lt_including ).

      mv_field_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_basetab_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = 'tabname'
                            iv_fieldname          = |{ c_base_table }~{ c_fields-viewname }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_basetab_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>dd26s }|
                      iv_alias      = c_base_tab_table
                      it_conditions = VALUE #( ( field           = c_fields-viewname
                                                 ref_field       = c_fields-viewname
                                                 ref_table_alias = c_base_table
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).

      add_option_filter( iv_fieldname = |{ c_base_tab_table }~tabname|
                         it_values    = lt_including ).

      mv_basetab_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
