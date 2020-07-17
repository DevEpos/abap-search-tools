"! <p class="shorttext synchronized" lang="en">Database table/view search provider for Object search</p>
CLASS zcl_sat_os_dbtab_provider DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_base_search_provider.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor.
  PROTECTED SECTION.
    METHODS determine_grouping
        REDEFINITION.
    METHODS prepare_search
        REDEFINITION.
  PRIVATE SECTION.
    ALIASES:
       c_dbtab_search_params FOR zif_sat_c_object_search~c_dbtab_search_params.
    CONSTANTS:
      c_base_table  TYPE string VALUE 'base',
      c_field_table TYPE string VALUE 'field',
      BEGIN OF c_fields,
        alias               TYPE string VALUE 'field' ##NO_TEXT,
        entityid            TYPE string VALUE 'entityid' ##NO_TEXT,
        type                TYPE string VALUE 'type' ##NO_TEXT,
        fieldname           TYPE string VALUE 'fieldname' ##NO_TEXT,
        description         TYPE string VALUE 'description' ##NO_TEXT,
        development_package TYPE string VALUE 'developmentpackage' ##NO_TEXT,
        delivery_class      TYPE string VALUE 'deliveryclass' ##no_text,
        created_by          TYPE string VALUE 'createdby' ##NO_TEXT,
        created_date        TYPE string VALUE 'createddate' ##NO_TEXT,
        changed_by          TYPE string VALUE 'changedby' ##NO_TEXT,
        changed_date        TYPE string VALUE 'changeddate' ##NO_TEXT,
      END OF c_fields.

    DATA mv_field_subquery TYPE string.
    DATA mv_field_filter_count TYPE i.
    DATA mv_entity_fieldname     TYPE string.
    DATA mv_raw_entity_fieldname TYPE string.

    "! <p class="shorttext synchronized" lang="en">Create filter for TYPE option</p>
    "!
    METHODS add_type_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FIELD option</p>
    METHODS add_field_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">Fills the names of base table and field</p>
    METHODS get_base_table_and_field
      EXPORTING
        ev_base_table           TYPE string
        ev_entity_fieldname     TYPE string
        ev_raw_entity_fieldname TYPE string.

ENDCLASS.



CLASS zcl_sat_os_dbtab_provider IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_field_subquery = |SELECT DISTINCT tablename | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_tablefield } | && c_cr_lf &&
                        | WHERE |.
  ENDMETHOD.

  METHOD prepare_search.
    DATA: lf_has_type_option TYPE abap_bool,
          lv_base_table      TYPE string.

    get_base_table_and_field(
      IMPORTING ev_base_table           = lv_base_table
                ev_entity_fieldname     = mv_entity_fieldname
                ev_raw_entity_fieldname = mv_raw_entity_fieldname
    ).

    set_base_select_table(
        iv_entity     = lv_base_table
        iv_alias      = c_base_table
        it_parameters = VALUE #(
          ( param_name = 'p_language' param_value = zcl_sat_system_helper=>get_system_language( ) )
        )
    ).

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

*.......... Find objects via its description
        WHEN c_general_search_options-description.
          add_option_filter(
            iv_fieldname = mv_description_filter_field
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects with a certain responsible person
        WHEN c_general_search_options-user.
          add_option_filter(
            iv_fieldname = 'createdby'
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects which exist in a certain development package
        WHEN c_general_search_options-package.
          add_option_filter(
            iv_fieldname = 'developmentpackage'
            it_values    = <ls_option>-value_range
          ).

*.......... Find only objects with a certain type
        WHEN c_general_search_options-type.
          add_type_option_filter( <ls_option>-value_range ).

*.......... Find objects by field
        WHEN c_dbtab_search_params-field.
          add_field_filter( <ls_option>-value_range ).

*.......... Find objects by delivery class
        WHEN c_dbtab_search_params-delivery_class.
          add_option_filter(
            iv_fieldname = c_fields-delivery_class
            it_values    = <ls_option>-value_range
          ).

      ENDCASE.
    ENDLOOP.

    IF mo_search_query->has_search_terms( ).
      add_search_terms_to_search( VALUE #( ( |{ c_base_table }~{ mv_entity_fieldname }| ) ) ).
    ENDIF.

    add_select_field( iv_fieldname = mv_entity_fieldname iv_fieldname_alias = 'entity_id' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = mv_raw_entity_fieldname iv_fieldname_alias = 'entity_id_raw' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'description' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'createdby' iv_fieldname_alias = 'created_by' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'developmentpackage' iv_fieldname_alias = 'devclass' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'type' iv_fieldname_alias = 'entity_type' iv_entity = c_base_table ).

    add_order_by( iv_fieldname = mv_entity_fieldname iv_entity = c_base_table  ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD determine_grouping.
    CHECK ms_search_engine_params-use_and_cond_for_options = abap_true.

***.. Excluding would break the relational division logic and would lead to unreliable results
***    CHECK mf_excluding_found = abap_false.

    IF NOT ( mv_field_filter_count > 1 ).
      RETURN.
    ENDIF.

    add_group_by_clause( |{ c_base_table }~{ mv_entity_fieldname }| ).
    add_group_by_clause( |{ c_base_table }~{ mv_raw_entity_fieldname }| ).
    add_group_by_clause( |{ c_base_table }~description| ).
    add_group_by_clause( |{ c_base_table }~createdby| ).
    add_group_by_clause( |{ c_base_table }~developmentpackage| ).
    add_group_by_clause( |{ c_base_table }~type| ).

    IF mv_field_filter_count > 1.
      add_having_clause( iv_field = |{ c_field_table }~fieldname| iv_counter_compare = mv_field_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD add_type_option_filter.
    DATA: lt_type_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.

    LOOP AT it_values INTO DATA(ls_value).
      CASE ls_value-low.

        WHEN zif_sat_c_object_search=>c_type_option_value-table.
          ls_value-low = zif_sat_c_entity_type=>table.

        WHEN zif_sat_c_object_search=>c_type_option_value-view.
          ls_value-low = zif_sat_c_entity_type=>view.

      ENDCASE.

      lt_type_filters = VALUE #( BASE lt_type_filters ( ls_value ) ).
    ENDLOOP.

    add_option_filter(
        iv_fieldname = 'type'
        it_values    = lt_type_filters
    ).
  ENDMETHOD.


  METHOD get_base_table_and_field.
    DATA: lf_set_default_table TYPE abap_bool.

    IF mo_search_query->has_options( ).
      DATA(ls_type_option) = mo_search_query->get_option( zif_sat_c_object_search=>c_search_option-by_type ).

      IF ls_type_option IS NOT INITIAL AND lines( ls_type_option-value_range ) = 1.
        DATA(ls_type_value) = ls_type_option-value_range[ 1 ].

        IF ls_type_value-low = zif_sat_c_object_search=>c_type_option_value-table.
          ev_base_table = zif_sat_c_select_source_id=>zsat_i_databasetable.
          ev_entity_fieldname =
          ev_raw_entity_fieldname = 'tablename'.
        ELSEIF ls_type_value-low = zif_sat_c_object_search=>c_type_option_value-view.
          ev_base_table = zif_sat_c_select_source_id=>zsat_i_databaseview.
          ev_entity_fieldname =
          ev_raw_entity_fieldname = 'viewname'.
        ENDIF.

      ELSE.
        lf_set_default_table = abap_true.
      ENDIF.
    ELSE.
      lf_set_default_table = abap_true.
    ENDIF.

    IF lf_set_default_table = abap_true.
      ev_base_table = zif_sat_c_select_source_id=>zsat_i_databasetablesandviews.
      ev_entity_fieldname = 'entity'.
      ev_raw_entity_fieldname = 'entityraw'.
    ENDIF.
  ENDMETHOD.


  METHOD add_field_filter.
    add_join_table(
        iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tablefield }|
        iv_alias      = |{ c_field_table }|
        it_conditions = VALUE #(
          ( field = 'tablename' ref_field = mv_entity_fieldname ref_table_alias = c_base_table type = zif_sat_c_join_cond_type=>field )
        )
    ).

    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    create_not_in_filter(
        iv_subquery_fieldname = 'fieldname'
        iv_fieldname          = |{ c_base_table }~tablename|
        it_excluding          = lt_excluding
        iv_subquery           = mv_field_subquery
    ).

    add_option_filter(
        iv_fieldname = |{ c_field_table }~fieldname|
        it_values    = it_values
    ).

    mv_field_filter_count = lines( it_values ).
  ENDMETHOD.

ENDCLASS.
