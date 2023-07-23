"! <p class="shorttext synchronized">Generic Searcher for Object Search</p>
CLASS zcl_sat_base_search_provider DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_join_cond_type.
    INTERFACES zif_sat_c_object_search.
    INTERFACES zif_sat_object_search_provider
      FINAL METHODS search.

    ALIASES c_field_cond  FOR zif_sat_c_join_cond_type~field.
    ALIASES c_filter_cond FOR zif_sat_c_join_cond_type~filter.

    METHODS constructor.

  PROTECTED SECTION.
    TYPES ty_search_term TYPE RANGE OF string.

    ALIASES c_general_search_options FOR zif_sat_c_object_search~c_general_search_params.

    CONSTANTS c_cr_lf TYPE string VALUE cl_abap_char_utilities=>cr_lf ##NO_TEXT.

    CONSTANTS:
      BEGIN OF c_result_fields,
        object_name          TYPE string VALUE 'OBJECT_NAME',
        raw_object_name      TYPE string VALUE 'RAW_OBJECT_NAME',
        alt_object_name      TYPE string VALUE 'ALT_OBJECT_NAME',
        entity_type          TYPE string VALUE 'ENTITY_TYPE',
        sub_object_name      TYPE string VALUE 'SUB_OBJECT_NAME',
        sub_object_type      TYPE string VALUE 'SUB_OBJECT_TYPE',
        tadir_type           TYPE string VALUE 'TADIR_TYPE',
        description          TYPE string VALUE 'DESCRIPTION',
        devclass             TYPE string VALUE 'DEVCLASS',
        api_state            TYPE string VALUE 'API_STATE',
        custom_field_short1  TYPE string VALUE 'CUSTOM_FIELD_SHORT1',
        custom_field_short2  TYPE string VALUE 'CUSTOM_FIELD_SHORT2',
        custom_field_short3  TYPE string VALUE 'CUSTOM_FIELD_SHORT3',
        custom_field_short4  TYPE string VALUE 'CUSTOM_FIELD_SHORT4',
        custom_field_short5  TYPE string VALUE 'CUSTOM_FIELD_SHORT5',
        custom_field_short6  TYPE string VALUE 'CUSTOM_FIELD_SHORT6',
        custom_field_short7  TYPE string VALUE 'CUSTOM_FIELD_SHORT7',
        custom_field_short8  TYPE string VALUE 'CUSTOM_FIELD_SHORT8',
        custom_field_short9  TYPE string VALUE 'CUSTOM_FIELD_SHORT9',
        custom_field_short10 TYPE string VALUE 'CUSTOM_FIELD_SHORT10',
        custom_field_medium1 TYPE string VALUE 'CUSTOM_FIELD_MEDIUM1',
        custom_field_medium2 TYPE string VALUE 'CUSTOM_FIELD_MEDIUM2',
        custom_field_medium3 TYPE string VALUE 'CUSTOM_FIELD_MEDIUM3',
        custom_field_medium4 TYPE string VALUE 'CUSTOM_FIELD_MEDIUM4',
        custom_field_medium5 TYPE string VALUE 'CUSTOM_FIELD_MEDIUM5',
        custom_field_long1   TYPE string VALUE 'CUSTOM_FIELD_LONG1',
        custom_field_long2   TYPE string VALUE 'CUSTOM_FIELD_LONG2',
        custom_field_long3   TYPE string VALUE 'CUSTOM_FIELD_LONG3',
        custom_field_long4   TYPE string VALUE 'CUSTOM_FIELD_LONG4',
        custom_field_long5   TYPE string VALUE 'CUSTOM_FIELD_LONG5',
        created_by           TYPE string VALUE 'CREATED_BY',
        created_date         TYPE string VALUE 'CREATED_DATE',
        changed_by           TYPE string VALUE 'CHANGED_BY',
        changed_date         TYPE string VALUE 'CHANGED_DATE',
      END OF c_result_fields.

    DATA mo_search_query             TYPE REF TO zif_sat_object_search_query.
    DATA ms_search_engine_params     TYPE zif_sat_ty_object_search=>ty_s_search_engine_params.
    DATA mt_result                   TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA mt_criteria                 TYPE zif_sat_ty_global=>ty_t_seltab_sql.
    DATA mt_criteria_or              TYPE zif_sat_ty_global=>ty_t_or_seltab_sql.
    DATA mt_criteria_and             TYPE zif_sat_ty_global=>ty_t_and_seltab_sql.
    DATA mt_where                    TYPE string_table.
    DATA mt_select                   TYPE string_table.
    DATA mt_order_by                 TYPE string_table.
    DATA mt_group_by                 TYPE string_table.
    DATA mt_having                   TYPE string_table.
    DATA mt_from                     TYPE TABLE OF string.
    DATA ms_join_def                 TYPE zif_sat_ty_global=>ty_s_join_def.
    DATA mf_excluding_found          TYPE abap_bool.
    DATA mv_description_filter_field TYPE string.

    "! <p class="shorttext synchronized">Start new criteria table connected with OR</p>
    METHODS new_or_cond_list.
    "! <p class="shorttext synchronized">Start new criteria table connected with AND</p>
    METHODS new_and_cond_list.

    "! <p class="shorttext synchronized">Starts the object search</p>
    METHODS search
      RAISING
        zcx_sat_object_search.

    "! <p class="shorttext synchronized">Get SQL name for CDS view</p>
    "! Depending on the current release the internal ddic sql views are taken
    "! instead of the cds view name
    METHODS get_cds_sql_name
      IMPORTING
        iv_entity_id        TYPE zsat_entity_id
      RETURNING
        VALUE(rv_ddic_view) TYPE tabname.

    "! <p class="shorttext synchronized">Adds the given select field</p>
    METHODS add_select_field
      IMPORTING
        iv_fieldname       TYPE string
        iv_fieldname_alias TYPE string OPTIONAL
        iv_entity          TYPE string OPTIONAL.

    "! <p class="shorttext synchronized">Add filter to the search</p>
    METHODS add_filter
      IMPORTING
        is_filter TYPE zif_sat_ty_global=>ty_s_seltab_sql.

    "! <p class="shorttext synchronized">Add base select entity</p>
    METHODS set_base_select_table
      IMPORTING
        iv_entity     TYPE string
        iv_alias      TYPE string                 OPTIONAL
        it_parameters TYPE zsat_table_parameter_t OPTIONAL.

    "! <p class="shorttext synchronized">Add Join table to the filter</p>
    METHODS add_join_table
      IMPORTING
        iv_join_table TYPE tabname
        iv_alias      TYPE string                 OPTIONAL
        iv_join_type  TYPE zsat_jointype          DEFAULT zif_sat_c_join_types=>inner_join
        it_parameters TYPE zsat_table_parameter_t OPTIONAL
        it_conditions TYPE zsat_join_condition_data_t.

    "! <p class="shorttext synchronized">Creates where condition table for given OR seltab</p>
    METHODS create_or_where_condition
      IMPORTING
        iv_fieldname    TYPE string
        iv_alt_option   TYPE ddoption OPTIONAL
        iv_alt_sign     TYPE ddsign   OPTIONAL
        it_values       TYPE zif_sat_ty_object_search=>ty_t_value_range
      RETURNING
        VALUE(rt_where) TYPE string_table.

    "! <p class="shorttext synchronized">Create FROM clause for SQL select</p>
    METHODS create_from_clause.
    "! <p class="shorttext synchronized">Create Where clause for SQL</p>
    METHODS create_where_clause.
    "! <p class="shorttext synchronized">Create Order By clause for SQL</p>
    METHODS create_order_by_clause.
    "! <p class="shorttext synchronized">Creates SELECT clause for SQL</p>
    METHODS create_select_clause.

    "! <p class="shorttext synchronized">Add filter(s) for search option</p>
    METHODS add_option_filter
      IMPORTING
        iv_fieldname    TYPE string
        iv_sql_function TYPE zsat_sql_function OPTIONAL
        it_values       TYPE zif_sat_ty_object_search=>ty_t_value_range.

    "! <p class="shorttext synchronized">Add subquery filter for field</p>
    METHODS add_subquery_filter
      IMPORTING
        iv_fieldname TYPE string
        iv_sign      TYPE ddsign DEFAULT zif_sat_c_options=>including
        iv_option    TYPE ddoption
        iv_subquery  TYPE string.

    "! <p class="shorttext synchronized">Adds new having clause for the given field</p>
    METHODS add_having_clause
      IMPORTING
        iv_field           TYPE string
        iv_counter_compare TYPE i.

    "! <p class="shorttext synchronized">Add sort order for field</p>
    METHODS add_order_by
      IMPORTING
        iv_fieldname  TYPE string
        iv_entity     TYPE string
        if_descending TYPE abap_bool OPTIONAL.

    "! <p class="shorttext synchronized">Adds field to GROUP BY clause</p>
    METHODS add_group_by_clause
      IMPORTING
        iv_field TYPE string.

    "! <p class="shorttext synchronized">Determine grouping state</p>
    METHODS determine_grouping.

    "! <p class="shorttext synchronized">Splits including/excluding from given value range list</p>
    METHODS split_including_excluding
      IMPORTING
        it_values    TYPE zif_sat_ty_object_search=>ty_t_value_range
      EXPORTING
        et_including TYPE zif_sat_ty_object_search=>ty_t_value_range
        et_excluding TYPE zif_sat_ty_object_search=>ty_t_value_range.

    "! <p class="shorttext synchronized">Creates NOT IN filter for the given fieldname</p>
    METHODS create_not_in_filter_for_where
      IMPORTING
        it_where     TYPE string_table
        iv_fieldname TYPE string
        iv_subquery  TYPE string.

    "! <p class="shorttext synchronized">Creates NOT IN where filter for the given fieldname</p>
    METHODS create_not_in_filter
      IMPORTING
        it_excluding          TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_subquery_fieldname TYPE string
        iv_subquery           TYPE string
        iv_fieldname          TYPE string.

    "! <p class="shorttext synchronized">Add search terms of query to search filter</p>
    METHODS add_search_terms_to_search
      IMPORTING
        iv_target      TYPE string OPTIONAL
        it_field_names TYPE string_table.

    "! <p class="shorttext synchronized">Performs task after the Search</p>
    METHODS do_after_search
      RAISING
        zcx_sat_object_search.

    "! <p class="shorttext synchronized">Fills the descriptions of the result</p>
    "!
    METHODS fill_descriptions.

    "! <p class="shorttext synchronized">Preparation tasks before actual search</p>
    "!
    METHODS prepare_search
      ABSTRACT
      RAISING
        zcx_sat_object_search.

    "! <p class="shorttext synchronized">Create filter for API option</p>
    METHODS add_api_option_filter
      IMPORTING
        it_values          TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_ref_field       TYPE fieldname
        iv_ref_table_alias TYPE string
        it_tadir_type      TYPE trobjtype_tab.

  PRIVATE SECTION.
    METHODS get_select_string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS add_select_part
      IMPORTING
        iv_part_name TYPE string
        it_part      TYPE ANY TABLE
      CHANGING
        cv_select    TYPE string.
ENDCLASS.


CLASS zcl_sat_base_search_provider IMPLEMENTATION.
  METHOD add_api_option_filter.
    CONSTANTS c_api_alias TYPE string VALUE 'api' ##NO_TEXT.

    DATA lt_api_filters   TYPE zif_sat_ty_object_search=>ty_t_value_range.
    DATA lt_state_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.

    DATA(lf_single_tadir_type) = xsdbool( lines( it_tadir_type ) = 1 ).

    DATA(lt_conditions) = VALUE zsat_join_condition_data_t( ( field           = 'objectname'
                                                              ref_field       = iv_ref_field
                                                              ref_table_alias = iv_ref_table_alias
                                                              type            = zif_sat_c_join_cond_type=>field ) ).

    IF lf_single_tadir_type = abap_true.
      lt_conditions = VALUE #( BASE lt_conditions
                               ( field         = 'objecttype'
                                 tabname_alias = CONV #( c_api_alias )
                                 value         = it_tadir_type[ 1 ]
                                 type          = zif_sat_c_join_cond_type=>filter ) ).
    ENDIF.

    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_apistates }|
                    iv_alias      = c_api_alias
                    it_conditions = lt_conditions ).

    LOOP AT it_values INTO DATA(ls_value).

      CASE ls_value-low.

        WHEN zif_sat_c_object_search=>c_api_option_value-released OR
             zif_sat_c_object_search=>c_api_option_value-deprecated.
          lt_state_filters = VALUE #( BASE lt_state_filters ( ls_value ) ).

        WHEN OTHERS.
          lt_api_filters = VALUE #( BASE lt_api_filters ( ls_value ) ).
      ENDCASE.

    ENDLOOP.

    IF lf_single_tadir_type = abap_false.
      add_option_filter( iv_fieldname = |{ c_api_alias }~objecttype|
                         it_values    = VALUE #( FOR type IN it_tadir_type
                                                 ( sign = 'I' option = 'EQ' low = type ) ) ).
    ENDIF.

    IF lt_api_filters IS NOT INITIAL.

      add_option_filter( iv_fieldname = |{ c_api_alias }~filtervalue|
                         it_values    = lt_api_filters ).
    ENDIF.

    IF lt_state_filters IS NOT INITIAL.
      add_option_filter( iv_fieldname = |{ c_api_alias }~apistate|
                         it_values    = lt_state_filters ).
    ENDIF.
  ENDMETHOD.

  METHOD add_filter.
    mt_criteria = VALUE #( BASE mt_criteria
                           ( is_filter ) ).

    IF is_filter-sign = zif_sat_c_options=>excluding.
      mf_excluding_found = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_group_by_clause.
    IF mt_group_by IS NOT INITIAL.
      DATA(lr_last_group_by) = REF #( mt_group_by[ lines( mt_group_by ) ] ).
      lr_last_group_by->* = |{ lr_last_group_by->* },|.
    ENDIF.

    mt_group_by = VALUE #( BASE mt_group_by ( iv_field ) ).
  ENDMETHOD.

  METHOD add_having_clause.
    DATA(lv_and_operator) = COND #( WHEN mt_having IS NOT INITIAL THEN |AND | ELSE |    | ).

    mt_having = VALUE #( BASE mt_having
                         ( |{ lv_and_operator }COUNT( DISTINCT { iv_field } ) >= { iv_counter_compare }| ) ).
  ENDMETHOD.

  METHOD add_join_table.
    ms_join_def-tables = VALUE #(
        BASE ms_join_def-tables
        ( add_table       = iv_join_table
          add_table_alias = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE iv_join_table )
          join_type       = iv_join_type
          conditions      = it_conditions
          parameters      = it_parameters ) ).
  ENDMETHOD.

  METHOD add_option_filter.
    mt_criteria = VALUE #( BASE mt_criteria
                           ( LINES OF VALUE #( FOR value IN it_values
                                               ( sqlfieldname = iv_fieldname
                                                 sql_function = iv_sql_function
                                                 sign         = value-sign
                                                 option       = value-option
                                                 low          = value-low ) ) ) ).

    IF mf_excluding_found = abap_true.
      RETURN.
    ENDIF.

    IF line_exists( it_values[ sign = zif_sat_c_options=>excluding ] ).
      mf_excluding_found = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_order_by.
    mt_order_by = VALUE #(
        BASE mt_order_by
        ( |{ iv_entity }~{ iv_fieldname } { COND #( WHEN if_descending = abap_true THEN 'DESCENDING' ELSE 'ASCENDING' ) }| ) ).
  ENDMETHOD.

  METHOD add_search_terms_to_search.
    DATA(lt_search_terms) = VALUE #( mo_search_query->mt_search_term[ target = iv_target ]-values OPTIONAL ).
    IF lt_search_terms IS INITIAL.
      RETURN.
    ENDIF.

    DEFINE _add_filter.
      add_filter( VALUE #( sqlfieldname = &1
                           option       = &2-option
                           sign         = &2-sign
                           low          = &2-low ) ).
    END-OF-DEFINITION.

    DEFINE _new_cond_list.
      IF &1 = abap_true.
        new_and_cond_list( ).
      ELSE.
        new_or_cond_list( ).
      ENDIF.
    END-OF-DEFINITION.

    " .. If at least one search term with 'E'(Excluding) sign exists all search terms are connected
    " .... via AND in the SQL Clause
    IF line_exists( lt_search_terms[ sign = zif_sat_c_options=>excluding ] ).
      DATA(lf_use_and_between_terms) = abap_true.
    ENDIF.

    new_and_cond_list( ).

    LOOP AT lt_search_terms ASSIGNING FIELD-SYMBOL(<ls_term>).
      DATA(lv_tabix) = sy-tabix.
      DATA(lf_and) = xsdbool( <ls_term>-sign = zif_sat_c_options=>excluding ).

      LOOP AT it_field_names INTO DATA(lv_fieldname).
        _add_filter lv_fieldname <ls_term>.

        CHECK sy-tabix <> lines( it_field_names ).
        _new_cond_list lf_and.
      ENDLOOP.

      IF lv_tabix <> lines( mo_search_query->mt_search_term ).
        _new_cond_list lf_use_and_between_terms.
      ENDIF.
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD add_select_field.
    mt_select = VALUE #( BASE mt_select
                         ( COND #( WHEN iv_entity IS NOT INITIAL THEN |{ iv_entity }~| ) &&
                           |{ iv_fieldname }| &&
                           COND #( WHEN iv_fieldname_alias IS NOT INITIAL THEN | AS { iv_fieldname_alias }| ) ) ).
  ENDMETHOD.

  METHOD add_select_part.
    FIELD-SYMBOLS <lv_part> TYPE any.

    CHECK it_part IS NOT INITIAL.

    cv_select = |{ cv_select }{ c_cr_lf }{ iv_part_name } |.
    LOOP AT it_part ASSIGNING <lv_part>.
      cv_select = |{ cv_select }{ c_cr_lf }    { <lv_part> }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_subquery_filter.
    mt_criteria = VALUE #( BASE mt_criteria
                           ( sqlfieldname = iv_fieldname
                             sign         = iv_sign
                             option       = iv_option
                             subquery     = iv_subquery ) ).
  ENDMETHOD.

  METHOD constructor.
    mv_description_filter_field = COND #( WHEN sy-saprl >= 751 THEN 'descriptionupper' ELSE 'description' ).
  ENDMETHOD.

  METHOD create_from_clause.
    mt_from = zcl_sat_join_helper=>build_from_clause_for_join_def( is_join_def = ms_join_def ).
  ENDMETHOD.

  METHOD create_not_in_filter.
    CHECK it_excluding IS NOT INITIAL.

    DATA(lt_excl_for_where) = create_or_where_condition( iv_fieldname = iv_subquery_fieldname
                                                         iv_alt_sign  = zif_sat_c_options=>including
                                                         it_values    = it_excluding ).

    DATA(lv_excluding_subquery) = iv_subquery.
    LOOP AT lt_excl_for_where ASSIGNING FIELD-SYMBOL(<ls_where_excl>).
      lv_excluding_subquery = |{ lv_excluding_subquery }{ c_cr_lf }{ <ls_where_excl> }|.
    ENDLOOP.

    add_subquery_filter( iv_fieldname = iv_fieldname
                         iv_subquery  = lv_excluding_subquery
                         iv_option    = zif_sat_c_options=>not_in_subquery ).
  ENDMETHOD.

  METHOD create_not_in_filter_for_where.
    DATA(lv_excluding_subquery) = iv_subquery.
    LOOP AT it_where ASSIGNING FIELD-SYMBOL(<ls_where_excl>).
      lv_excluding_subquery = |{ lv_excluding_subquery }{ c_cr_lf }{ <ls_where_excl> }|.
    ENDLOOP.

    add_subquery_filter( iv_fieldname = iv_fieldname
                         iv_subquery  = lv_excluding_subquery
                         iv_option    = zif_sat_c_options=>not_in_subquery ).
  ENDMETHOD.

  METHOD create_order_by_clause.
    CHECK: mt_order_by IS NOT INITIAL,
           lines( mt_order_by ) > 1.

    DATA(lv_line_count) = lines( mt_order_by ).
    LOOP AT mt_order_by ASSIGNING FIELD-SYMBOL(<lv_order_by>).
      IF sy-tabix <> lv_line_count.
        <lv_order_by> = <lv_order_by> && |, |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_or_where_condition.
    DATA(lt_options) = VALUE zif_sat_ty_global=>ty_t_seltab_sql(
        ( LINES OF VALUE #( FOR value IN it_values
                            LET opt  = COND #( WHEN iv_alt_option IS NOT INITIAL THEN iv_alt_option ELSE value-option )
                                sign = COND #( WHEN iv_alt_sign IS NOT INITIAL THEN iv_alt_sign ELSE value-sign ) IN
                            ( sqlfieldname = iv_fieldname
                              sign         = sign
                              option       = opt
                              low          = value-low ) ) ) ).

    DATA(lt_or_seltab) = VALUE zif_sat_ty_global=>ty_t_or_seltab_sql( ( values = lt_options ) ).
    rt_where = zcl_sat_where_clause_builder=>create_or_condition( lt_or_seltab ).
  ENDMETHOD.

  METHOD create_select_clause.
    CHECK: mt_select IS NOT INITIAL,
           lines( mt_select ) > 1.

    DATA(lv_line_count) = lines( mt_select ).
    LOOP AT mt_select ASSIGNING FIELD-SYMBOL(<lv_select>).
      IF sy-tabix <> lv_line_count.
        <lv_select> = <lv_select> && |, |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_where_clause.
    CHECK mt_criteria_and IS NOT INITIAL.

    mt_where = zcl_sat_where_clause_builder=>create_and_condition( it_and_seltab = mt_criteria_and ).
  ENDMETHOD.

  METHOD determine_grouping.
    RETURN.
  ENDMETHOD.

  METHOD do_after_search.
    RETURN.
  ENDMETHOD.

  METHOD fill_descriptions.
    DATA lt_texts TYPE STANDARD TABLE OF seu_objtxt.

    CHECK mt_result IS NOT INITIAL.

    lt_texts = VALUE #( FOR entity IN mt_result
                        WHERE
                              ( tadir_type IS NOT INITIAL AND object_name IS NOT INITIAL )
                        ( object = entity-tadir_type obj_name = entity-object_name ) ).
    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES obj_tab = lt_texts.

    LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<ls_entity>).
      ASSIGN lt_texts[ obj_name = <ls_entity>-object_name ] TO FIELD-SYMBOL(<ls_text>).
      IF sy-subrc = 0.
        <ls_entity>-description = <ls_text>-stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_cds_sql_name.
    rv_ddic_view = zcl_sat_cds_view_factory=>read_ddl_ddic_view( iv_ddl_name = |{ iv_entity_id }| ).
  ENDMETHOD.

  METHOD get_select_string.
    add_select_part( EXPORTING iv_part_name = 'SELECT DISTINCT'
                               it_part      = mt_select
                     CHANGING  cv_select    = rv_result ).
    add_select_part( EXPORTING iv_part_name = 'FROM'
                               it_part      = mt_from
                     CHANGING  cv_select    = rv_result ).
    add_select_part( EXPORTING iv_part_name = 'WHERE'
                               it_part      = mt_where
                     CHANGING  cv_select    = rv_result ).
    add_select_part( EXPORTING iv_part_name = 'GROUP BY'
                               it_part      = mt_group_by
                     CHANGING  cv_select    = rv_result ).
    add_select_part( EXPORTING iv_part_name = 'HAVING'
                               it_part      = mt_having
                     CHANGING  cv_select    = rv_result ).
    add_select_part( EXPORTING iv_part_name = 'ORDER BY'
                               it_part      = mt_order_by
                     CHANGING  cv_select    = rv_result ).
  ENDMETHOD.

  METHOD new_and_cond_list.
    IF mt_criteria_or IS NOT INITIAL.
      IF mt_criteria IS NOT INITIAL.
        new_or_cond_list( ).
      ENDIF.
      mt_criteria_and = VALUE #( BASE mt_criteria_and ( VALUE #( ( LINES OF mt_criteria_or ) ) ) ).
      CLEAR mt_criteria_or.
    ELSEIF mt_criteria IS NOT INITIAL.
      mt_criteria_and = VALUE #( BASE mt_criteria_and ( VALUE #( ( values = mt_criteria ) ) ) ).
      CLEAR mt_criteria.
    ENDIF.
  ENDMETHOD.

  METHOD new_or_cond_list.
    IF mt_criteria IS NOT INITIAL.
      mt_criteria_or = VALUE #( BASE mt_criteria_or ( values = mt_criteria ) ).
      CLEAR mt_criteria.
    ENDIF.
  ENDMETHOD.

  METHOD search.
    create_select_clause( ).
    create_from_clause( ).
    create_where_clause( ).
    create_order_by_clause( ).
    determine_grouping( ).

    DATA(lv_max_rows) = COND #( WHEN ms_search_engine_params-get_all = abap_true
                                THEN 0
                                ELSE mo_search_query->mv_max_rows + 1 ).

    TRY.
        IF mt_group_by IS NOT INITIAL.
          SELECT DISTINCT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            GROUP BY (mt_group_by)
            HAVING (mt_having)
            ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ELSE.
          SELECT DISTINCT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            ORDER BY (mt_order_by)
          INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ENDIF.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(lv_sql) = get_select_string( ).
      CATCH cx_sy_open_sql_error INTO DATA(lx_sql_error).
        RAISE EXCEPTION TYPE zcx_sat_object_search
          EXPORTING previous = lx_sql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD set_base_select_table.
    ms_join_def-primary_table       = iv_entity.
    ms_join_def-primary_table_alias = COND #( WHEN iv_alias IS NOT INITIAL THEN iv_alias ELSE iv_entity ).
    ms_join_def-parameters          = it_parameters.
  ENDMETHOD.

  METHOD split_including_excluding.
    et_excluding = VALUE #( FOR excluding IN it_values
                            WHERE ( sign = zif_sat_c_options=>excluding OR sign2 = zif_sat_c_options=>excluding )
                            ( excluding ) ).
    et_including = VALUE #( FOR including IN it_values
                            WHERE ( sign = zif_sat_c_options=>including AND ( sign2 = zif_sat_c_options=>including OR sign2 = space ) )
                            ( including ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_provider~search.
    mo_search_query = io_query.
    ms_search_engine_params = is_search_engine_params.

    CLEAR: mt_result,
           mt_criteria,
           mt_criteria_or,
           mt_criteria_and,
           mt_where,
           mt_select,
           mt_order_by,
           mt_group_by,
           mt_having,
           mt_from,
           ms_join_def,
           mf_excluding_found.
    " .. Prepare projection fields, filters, order by, etc.
    prepare_search( ).
    search( ).
    do_after_search( ).
    et_result = mt_result.
  ENDMETHOD.
ENDCLASS.
