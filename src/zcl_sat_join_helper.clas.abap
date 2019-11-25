"! <p class="shorttext synchronized" lang="en">Helper for table joins</p>
CLASS zcl_sat_join_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Join Condition Type</p>
      "! Internal type for Join Condition which is used for building the
      "! FROM clause
      BEGIN OF ty_join_condition,
        join_operator   TYPE string,
        open_bracket    TYPE string,
        value           TYPE string,
        closing_bracket TYPE string,
      END OF ty_join_condition .
    TYPES:
      tt_join_conditions TYPE STANDARD TABLE OF ty_join_condition WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en">Builds FROM SQL clause for join definition</p>
    CLASS-METHODS build_from_clause_for_join_def
      IMPORTING
        !is_join_def          TYPE zif_sat_ty_global=>ty_s_join_def OPTIONAL
        if_use_ddl_for_select TYPE abap_bool OPTIONAL
        !it_table_alias_map   TYPE zsat_table_to_alias_map_itab OPTIONAL
      RETURNING
        VALUE(rt_from_clause) TYPE string_table .
    "! <p class="shorttext synchronized" lang="en">Builds where clause for given conditions</p>
    CLASS-METHODS build_where_for_conditions
      IMPORTING
        it_conditions   TYPE tt_join_conditions
      RETURNING
        VALUE(rt_where) TYPE string_table .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_join_table,
        join_type  TYPE string,
        table      TYPE string,
        conditions TYPE tt_join_conditions,
      END OF ty_join_table .
    TYPES:
      tt_join_table TYPE STANDARD TABLE OF ty_join_table WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_join,
        primary_table TYPE string,
        tables        TYPE tt_join_table,
      END OF ty_join .

    "! <p class="shorttext synchronized" lang="en">Fills missing operators, etc.</p>
    CLASS-METHODS repair_join_definition
      IMPORTING
        !is_join_def          TYPE zif_sat_ty_global=>ty_s_join_def
        if_use_ddl_for_select TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_join_def)    TYPE zif_sat_ty_global=>ty_s_join_def .
    "! <p class="shorttext synchronized" lang="en">Prepares join table definitions for building FROM Strings</p>
    CLASS-METHODS prepare_tables
      IMPORTING
        !is_join_def   TYPE zif_sat_ty_global=>ty_s_join_def
      RETURNING
        VALUE(rs_join) TYPE ty_join .
    "! <p class="shorttext synchronized" lang="en">Append parameters string to table clause</p>
    CLASS-METHODS append_params_to_table_string
      IMPORTING
        it_parameters TYPE zsat_table_parameter_t
      CHANGING
        cv_table_part TYPE string.
ENDCLASS.



CLASS zcl_sat_join_helper IMPLEMENTATION.



  METHOD build_from_clause_for_join_def.
    DATA(ls_join_def) = repair_join_definition(
       if_use_ddl_for_select = if_use_ddl_for_select
       is_join_def           = is_join_def
    ).

*.. parse join definition for building the FROM Clause string
    DATA(ls_join_enriched) = prepare_tables(
        is_join_def           = ls_join_def
    ).

    rt_from_clause = VALUE #(
      ( ls_join_enriched-primary_table )
    ).

    LOOP AT ls_join_enriched-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
      rt_from_clause = VALUE #( BASE rt_from_clause
        ( |{ <ls_table>-join_type }{ <ls_table>-table }| )
      ).

      LOOP AT <ls_table>-conditions ASSIGNING FIELD-SYMBOL(<ls_cond>).
        rt_from_clause = VALUE #( BASE rt_from_clause
          ( |{ <ls_cond>-join_operator }{ <ls_cond>-open_bracket }{ <ls_cond>-value }{ <ls_cond>-closing_bracket }| )
        ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_where_for_conditions.
    LOOP AT it_conditions ASSIGNING FIELD-SYMBOL(<ls_cond>).
      rt_where = VALUE #( BASE rt_where
        ( |{ <ls_cond>-join_operator }{ <ls_cond>-open_bracket }{ <ls_cond>-value }{ <ls_cond>-closing_bracket }| )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD prepare_tables.
    DATA: lv_on_string          TYPE string,
          lv_and_string         TYPE string,
          lv_or_string          TYPE string,

          lf_parenthesis_opened TYPE abap_bool,
          lv_parenthesis_open   TYPE string,
          lv_parenthesis_closed TYPE string,

          lv_reference_alias    TYPE string,

          ls_new_condition      TYPE ty_join_condition,
          lv_index              TYPE sy-tabix,
          lv_previous_join_cond TYPE string.

    FIELD-SYMBOLS: <ls_last_condition> TYPE ty_join_condition.

*... prefill some needed join condition operator strings
    lv_on_string = |  { zif_sat_c_selection_condition=>on  ALIGN = RIGHT WIDTH = 5 } |.
    lv_and_string = |  { zif_sat_c_selection_condition=>and  ALIGN = RIGHT WIDTH = 5 } |.
    lv_or_string = |  { zif_sat_c_selection_condition=>or  ALIGN = RIGHT WIDTH = 5 } |.

    rs_join-primary_table = is_join_def-primary_table.

    IF is_join_def-parameters IS NOT INITIAL.
      append_params_to_table_string(
        EXPORTING it_parameters = is_join_def-parameters
        CHANGING  cv_table_part = rs_join-primary_table
      ).
    ENDIF.

    rs_join-primary_table = rs_join-primary_table && | AS | &&
                            COND #( WHEN is_join_def-primary_table_alias IS NOT INITIAL THEN
                                        is_join_def-primary_table_alias
                                  ).

*... build joins for defined join tables
    LOOP AT is_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_join_table>) WHERE is_virtual = abap_false.

      DATA(lv_join_table_alias) = COND #( WHEN <ls_join_table>-add_table_alias IS NOT INITIAL THEN
                                            <ls_join_table>-add_table_alias ).

      DATA(lv_join_type_text) = SWITCH string(
        <ls_join_table>-join_type
        WHEN zif_sat_c_join_types=>inner_join THEN |INNER JOIN |
        WHEN zif_sat_c_join_types=>left_outer_join THEN |LEFT OUTER JOIN |
        WHEN zif_sat_c_join_types=>right_outer_join THEN |RIGHT OUTER JOIN |
      ).

      DATA(ls_parsed_table) = VALUE ty_join_table(
          join_type  = lv_join_type_text
          table      = |{ <ls_join_table>-add_table }|
      ).

      IF <ls_join_table>-parameters IS NOT INITIAL.
        append_params_to_table_string(
          EXPORTING it_parameters = <ls_join_table>-parameters
          CHANGING  cv_table_part = ls_parsed_table-table
        ).
      ENDIF.

      ls_parsed_table-table = |{ ls_parsed_table-table } AS { lv_join_table_alias }|.

*.... parse the conditions
      LOOP AT <ls_join_table>-conditions INTO DATA(ls_cond).
        lv_index = sy-tabix.

*...... Determine the alias of the reference field
        IF ls_cond-type = zif_sat_c_join_cond_type=>field.
          lv_reference_alias = COND #( WHEN ls_cond-ref_table_alias IS NOT INITIAL THEN
                                         ls_cond-ref_table_alias
                                       WHEN ls_cond-ref_table = is_join_def-primary_table THEN
                                         is_join_def-primary_table_alias
                                       ELSE
                                         is_join_def-tables[ add_table = ls_cond-ref_table ]-add_table_alias ).
        ELSE.

          lv_reference_alias = COND #( WHEN ls_cond-tabname_alias IS NOT INITIAL THEN
                                         ls_cond-tabname_alias
                                       WHEN ls_cond-tabname = is_join_def-primary_table THEN
                                         is_join_def-primary_table_alias
                                       WHEN ls_cond-tabname IS NOT INITIAL THEN
                                         is_join_def-tables[ add_table = ls_cond-tabname ]-add_table_alias ).
        ENDIF.

*...... check for need of possible open parenthesis
        IF ls_cond-and_or = zif_sat_c_selection_condition=>or.
          IF lf_parenthesis_opened = abap_false.
            lv_parenthesis_open = |( |.
            lf_parenthesis_opened = abap_true.
          ENDIF.
        ELSE.
          IF lf_parenthesis_opened = abap_true.
            lv_parenthesis_closed = | )|.
            lf_parenthesis_opened = abap_false.
          ENDIF.
        ENDIF.

        IF ls_cond-type = zif_sat_c_join_cond_type=>field.
          ls_parsed_table-conditions = VALUE #( BASE ls_parsed_table-conditions
            ( open_bracket   = lv_parenthesis_open
              join_operator  = COND #( WHEN lv_index = 1 THEN lv_on_string ELSE lv_previous_join_cond )
              value          = |{ lv_join_table_alias }~{ ls_cond-field } { ls_cond-operator } | &&
                               |{ lv_reference_alias }~{ ls_cond-ref_field }|
              closing_bracket = lv_parenthesis_closed )
          ).
        ELSE.
          DATA(lv_value1) = |{ ls_cond-value }|.
          DATA(lv_value2) = ||.

*........ handle LIKE and NOT LIKE operator
          IF  ( ls_cond-operator = zif_sat_c_operator=>like OR
                ls_cond-operator = zif_sat_c_operator=>not_like ) AND
              ls_cond-value_type <> zif_sat_c_join_cond_val_type=>system_value_input.
***          ls_filter_cond-value = replace( val = ls_filter_cond-value sub = '*' with = '%' occ = 0 ).
*.......... Use proper SAP to SQL conversion for pattern
            zcl_sat_like_pattern_convrter=>conv_sap_to_sql_pattern(
              EXPORTING  iv_sap_pattern   = lv_value1
              IMPORTING  ev_sql_pattern   = lv_value1
                         ef_escape_needed = DATA(lf_escape_needed)
              EXCEPTIONS OTHERS = 1
            ).
            lv_value1 = cl_abap_dyn_prg=>quote( lv_value1 ).
            IF lf_escape_needed = abap_true.
              lv_value1 = |{ lv_value1 } ESCAPE '#' |.
            ENDIF.

            CLEAR ls_cond-value2.
          ELSE.
            IF ls_cond-value_type = zif_sat_c_join_cond_val_type=>system_value_input.
              IF lv_value1 = 'SY-LANGU'.
                lv_value1 = cl_abap_dyn_prg=>quote( zcl_sat_system_helper=>get_system_language( ) ).
              ELSE.
                lv_value1 = |@{ lv_value1 }|.
              ENDIF.
            ELSE.
              lv_value1 = cl_abap_dyn_prg=>quote( lv_value1 ).
            ENDIF.
          ENDIF.

          IF ls_cond-operator = 'BETWEEN'.
            lv_value2 = COND #(
              WHEN ls_cond-value_type = zif_sat_c_join_cond_val_type=>system_value_input THEN
                |@{ ls_cond-value2 }|
              ELSE
                cl_abap_dyn_prg=>quote( ls_cond-value2 )
            ).
            lv_value2 = | { zif_sat_c_selection_condition=>and } { lv_value2 }|.
          ENDIF.

          ls_parsed_table-conditions = VALUE #( BASE ls_parsed_table-conditions
            ( open_bracket    = lv_parenthesis_open
              join_operator   = COND #( WHEN lv_index = 1 THEN lv_on_string ELSE lv_previous_join_cond )
              value           = |{ lv_reference_alias }~{ ls_cond-field }| &&
                                | { ls_cond-operator } | &&
                                |{ lv_value1 }{ lv_value2 }|
              closing_bracket = lv_parenthesis_closed )
          ).
        ENDIF.

        lv_previous_join_cond = COND #( WHEN ls_cond-and_or = zif_sat_c_selection_condition=>or THEN lv_or_string ELSE lv_and_string ).
        CLEAR: lv_parenthesis_closed,
               lv_parenthesis_open.

        IF lf_parenthesis_opened = abap_true.
          lv_parenthesis_open = |  |.
        ENDIF.
      ENDLOOP.

      rs_join-tables = VALUE #( BASE rs_join-tables ( ls_parsed_table ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD repair_join_definition.
    FIELD-SYMBOLS: <ls_condition> TYPE zsat_join_condition_data.

    rs_join_def = is_join_def.
    IF is_join_def-primary_table_entity_type = zif_sat_c_entity_type=>cds_view AND
       ( if_use_ddl_for_select = abap_true OR sy-saprl < 750 ).

      rs_join_def-primary_table = zcl_sat_cds_view_factory=>read_ddl_ddic_view_for_entity( is_join_def-primary_table ).
    ENDIF.

    LOOP AT rs_join_def-tables ASSIGNING FIELD-SYMBOL(<ls_table>).
*.... fill unified condition table
      IF <ls_table>-conditions IS INITIAL.
*....... First the field conditions are added
        LOOP AT <ls_table>-field_conditions ASSIGNING FIELD-SYMBOL(<ls_field_cond>).
          APPEND INITIAL LINE TO <ls_table>-conditions ASSIGNING <ls_condition>.
          <ls_condition> = CORRESPONDING #( <ls_field_cond> ).
          <ls_condition>-type = zif_sat_c_join_cond_type=>field.
          <ls_condition>-and_or = zif_sat_c_selection_condition=>and.
        ENDLOOP.
*........ and then the filter conditions
        LOOP AT <ls_table>-filter_conditions ASSIGNING FIELD-SYMBOL(<ls_filter_cond>).
          APPEND INITIAL LINE TO <ls_table>-conditions ASSIGNING <ls_condition>.
          <ls_condition> = CORRESPONDING #( <ls_filter_cond> MAPPING field = fieldname ).
          <ls_condition>-type = zif_sat_c_join_cond_type=>filter.
        ENDLOOP.
*...... remove the last condition operator
        <ls_table>-conditions[ lines( <ls_table>-conditions ) ]-and_or = space.
      ENDIF.
      IF <ls_table>-entity_type = zif_sat_c_entity_type=>cds_view AND
         ( if_use_ddl_for_select = abap_true OR sy-saprl < 750 ).
        <ls_table>-add_table = zcl_sat_cds_view_factory=>read_ddl_ddic_view_for_entity( <ls_table>-add_table ).
      ENDIF.

      IF <ls_table>-join_type IS INITIAL.
        <ls_table>-join_type = zif_sat_c_join_types=>inner_join.
      ENDIF.

      LOOP AT <ls_table>-conditions ASSIGNING <ls_condition>.
        IF <ls_condition>-type IS INITIAL.
          <ls_condition>-type = zif_sat_c_join_cond_type=>field.
        ENDIF.
        IF <ls_condition>-operator IS INITIAL.
          <ls_condition>-operator = zif_sat_c_operator=>equals.
        ENDIF.
*...... Not quite sure if this can't still result in an erroneous select
        IF <ls_condition>-tabname IS INITIAL AND <ls_condition>-tabname_alias IS INITIAL.
          <ls_condition>-tabname = <ls_table>-table_name.
        ENDIF.

        IF <ls_condition>-value_type IS INITIAL.
          <ls_condition>-value_type = zif_sat_c_join_cond_val_type=>typed_input.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_params_to_table_string.
    cv_table_part = REDUCE string(
       INIT value = |{ cv_table_part }( | sep = ``
       FOR param IN it_parameters
       NEXT value = |{ value }{ sep }{ param-param_name } = { cl_abap_dyn_prg=>quote( param-param_value ) }| sep = `, `
    ) && | )|.
  ENDMETHOD.

ENDCLASS.
