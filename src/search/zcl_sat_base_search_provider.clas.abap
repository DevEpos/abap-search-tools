"! <p class="shorttext synchronized">Generic Searcher for Object Search</p>
CLASS zcl_sat_base_search_provider DEFINITION
  PUBLIC ABSTRACT
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
    TYPES:
      BEGIN OF ty_search_term_field,
        fieldname     TYPE string,
        null_possible TYPE abap_bool,
      END OF ty_search_term_field,
      ty_search_term_fields TYPE STANDARD TABLE OF ty_search_term_field WITH EMPTY KEY.

    ALIASES c_general_search_options FOR zif_sat_c_object_search~c_general_search_params.

    CONSTANTS c_cr_lf TYPE string VALUE cl_abap_char_utilities=>cr_lf ##NO_TEXT.

    CONSTANTS:
      BEGIN OF c_general_fields,
        created_by TYPE string VALUE 'createdby',
        created_on TYPE string VALUE 'createdon',
        changed_on TYPE string VALUE 'changedon',
        changed_by TYPE string VALUE 'changedby',
      END OF c_general_fields.

    CONSTANTS:
      BEGIN OF c_result_fields,
        object_name        TYPE string VALUE 'OBJECT_NAME',
        raw_object_name    TYPE string VALUE 'RAW_OBJECT_NAME',
        alt_object_name    TYPE string VALUE 'ALT_OBJECT_NAME',
        entity_type        TYPE string VALUE 'ENTITY_TYPE',
        tadir_type         TYPE string VALUE 'TADIR_TYPE',
        description        TYPE string VALUE 'DESCRIPTION',
        devclass           TYPE string VALUE 'DEVCLASS',
        api_state          TYPE string VALUE 'API_STATE',
        cds_source_type    TYPE string VALUE 'CDS_SOURCE_TYPE',
        message_number     TYPE string VALUE 'MESSAGE_NUMBER',
        message_short_text TYPE string VALUE 'MESSAGE_SHORT_TEXT',
        method_type        TYPE string VALUE 'METHOD_TYPE',
        method_status      TYPE string VALUE 'METHOD_STATUS',
        method_name        TYPE string VALUE 'METHOD_NAME',
        "! Declaring Class/Interface Name of method
        method_decl_clif   TYPE string VALUE 'METHOD_DECL_CLIF',
        "! Method name in original Class/Interface
        method_decl_method TYPE string VALUE 'METHOD_DECL_METHOD',
        method_is_abstract TYPE string VALUE 'METHOD_IS_ABSTRACT',
        method_is_final    TYPE string VALUE 'METHOD_IS_FINAL',
        method_exposure    TYPE string VALUE 'METHOD_EXPOSURE',
        method_level       TYPE string VALUE 'METHOD_LEVEL',
        method_descr       TYPE string VALUE 'METHOD_DESCR',
        created_by         TYPE string VALUE 'CREATED_BY',
        created_date       TYPE string VALUE 'CREATED_DATE',
        changed_by         TYPE string VALUE 'CHANGED_BY',
        changed_date       TYPE string VALUE 'CHANGED_DATE',
      END OF c_result_fields.

    DATA mo_search_query TYPE REF TO zif_sat_object_search_query.
    DATA ms_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params.
    DATA mt_result TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA mt_criteria TYPE zif_sat_ty_global=>ty_t_seltab_sql.
    DATA mt_criteria_or TYPE zif_sat_ty_global=>ty_t_or_seltab_sql.
    DATA mt_criteria_and TYPE zif_sat_ty_global=>ty_t_and_seltab_sql.
    DATA mt_where TYPE string_table.
    DATA mt_select TYPE string_table.
    DATA mt_order_by TYPE string_table.
    DATA mt_group_by TYPE string_table.
    DATA mt_having TYPE string_table.
    DATA mt_from TYPE TABLE OF string.
    DATA ms_join_def TYPE zif_sat_ty_global=>ty_s_join_def.
    DATA mv_description_filter_field TYPE string.
    DATA mt_fields_for_grouping TYPE string_table.

    "! <p class="shorttext synchronized">Start new criteria table connected with OR</p>
    METHODS new_or_cond_list.
    "! <p class="shorttext synchronized">Start new criteria table connected with AND</p>
    METHODS new_and_cond_list.

    METHODS new_cond_list
      IMPORTING
        if_and TYPE abap_bool.

    "! <p class="shorttext synchronized">Starts the object search</p>
    METHODS execute_sql_query
      RAISING
        zcx_sat_object_search.

    "! <p class="shorttext synchronized">Adds the given select field</p>
    METHODS add_select_field
      IMPORTING
        iv_fieldname       TYPE string
        iv_fieldname_alias TYPE string    OPTIONAL
        iv_entity          TYPE string    OPTIONAL
        if_no_grouping     TYPE abap_bool OPTIONAL.

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

    METHODS add_date_filter
      IMPORTING
        iv_fieldname TYPE string
        it_values    TYPE zif_sat_ty_object_search=>ty_t_value_range.

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

    "! Should return abap_true if group by clause is required
    "! Note: Subclass may adjust but should call super
    METHODS is_grouping_required
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Adds group by clauses to query
    METHODS add_group_by_clauses.

    "! Adds having clauses to query<br/>
    "! See ADD_HAVING_CLAUSE
    METHODS add_having_clauses.

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
        iv_target       TYPE string                               OPTIONAL
        it_search_terms TYPE zif_sat_ty_global=>ty_t_string_range OPTIONAL
        it_fields       TYPE ty_search_term_fields.

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

    "! <p class="shorttext synchronized">Creates filter for application component</p>
    METHODS add_appl_comp_filter
      IMPORTING
        it_values          TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_ref_field       TYPE fieldname
        iv_ref_table_alias TYPE string.

    METHODS add_softw_comp_filter
      IMPORTING
        it_values          TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_ref_field       TYPE fieldname
        iv_ref_table_alias TYPE string.

    METHODS add_package_filter
      IMPORTING
        it_values    TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_fieldname TYPE string.

    METHODS set_distinct_required.
    "! Manual switch to indicate that filters are available
    "! and the provider should execute the search
    METHODS set_filters_active.
    METHODS reset.

    METHODS contains_single_i_eq_filter
      IMPORTING
        it_values     TYPE zif_sat_ty_object_search=>ty_t_value_range
      RETURNING
        VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES ty_package_range TYPE RANGE OF devclass.

    CONSTANTS c_devc_tab_alias TYPE string VALUE 'devclass'.

    DATA mf_devclass_join_added TYPE abap_bool.
    DATA mf_distinct_required TYPE abap_bool.
    DATA mf_grouping_required TYPE abap_bool.
    DATA mf_excluding_found TYPE abap_bool.
    DATA mf_filters_active TYPE abap_bool.
    DATA mo_logger TYPE REF TO zcl_sat_os_logger.

    METHODS get_select_string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS add_select_part
      IMPORTING
        iv_part_name TYPE string
        it_part      TYPE ANY TABLE
      CHANGING
        cv_select    TYPE string.

    METHODS search_possible
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS add_devclass_join
      IMPORTING
        iv_ref_table_alias TYPE string
        iv_ref_field       TYPE fieldname.

    METHODS resolve_package_hierarchy
      IMPORTING
        it_values     TYPE zif_sat_ty_object_search=>ty_t_value_range
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS log_sql_info.
    METHODS log_success.
    METHODS log_select_success.

    METHODS log_select_error
      IMPORTING
        iv_error TYPE string.
ENDCLASS.


CLASS zcl_sat_base_search_provider IMPLEMENTATION.
  METHOD constructor.
    mv_description_filter_field = 'descriptionupper'.
  ENDMETHOD.

  METHOD zif_sat_object_search_provider~search.
    mo_logger = NEW #( io_query ).
    mo_search_query = io_query.
    ms_search_engine_params = is_search_engine_params.

    reset( ).
    prepare_search( ).

    IF search_possible( ).
      execute_sql_query( ).

      mo_logger->start_timer( ).
      do_after_search( ).
      log_success( ).

      et_result = mt_result.
    ENDIF.

    CLEAR mo_logger.
    reset( ).
  ENDMETHOD.

  METHOD execute_sql_query.
    create_select_clause( ).
    create_from_clause( ).
    create_where_clause( ).
    create_order_by_clause( ).

    IF ms_search_engine_params-use_and_cond_for_options = abap_true AND is_grouping_required( ).
      add_group_by_clauses( ).
      add_having_clauses( ).
    ENDIF.

    DATA(lv_max_rows) = COND #( WHEN ms_search_engine_params-get_all = abap_true
                                THEN 0
                                ELSE mo_search_query->mv_max_rows + 1 ).

    log_sql_info( ).
    mo_logger->start_timer( ).

    TRY.
        IF mt_group_by IS NOT INITIAL.
          SELECT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            GROUP BY (mt_group_by)
            HAVING (mt_having)
            ORDER BY (mt_order_by)
            INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ELSEIF mf_distinct_required = abap_true.
          SELECT DISTINCT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            ORDER BY (mt_order_by)
            INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ELSE.
          SELECT (mt_select)
            FROM (mt_from)
            WHERE (mt_where)
            ORDER BY (mt_order_by)
            INTO CORRESPONDING FIELDS OF TABLE @mt_result
            UP TO @lv_max_rows ROWS.
        ENDIF.

        log_select_success( ).
      CATCH cx_sy_open_sql_error INTO DATA(lx_sql_error).
        log_select_error( lx_sql_error->get_text( ) ).

        RAISE EXCEPTION TYPE zcx_sat_object_search
          EXPORTING previous = lx_sql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD add_api_option_filter.
    DATA(lo_api_state_filter_helper) = NEW lcl_api_state_filter_helper( io_search_provider = me
                                                                        it_values          = it_values
                                                                        iv_ref_field       = iv_ref_field
                                                                        iv_ref_table_alias = iv_ref_table_alias
                                                                        it_tadir_type      = it_tadir_type ).
    lo_api_state_filter_helper->create_api_filter( ).

    IF     ms_search_engine_params-use_and_cond_for_options = abap_true
       AND lo_api_state_filter_helper->has_including_filters( ).
      mf_grouping_required = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_appl_comp_filter.
    CHECK it_values IS NOT INITIAL.

    add_devclass_join( iv_ref_table_alias = iv_ref_table_alias
                       iv_ref_field       = iv_ref_field ).

    add_option_filter( iv_fieldname = |{ c_devc_tab_alias }~applicationcomponent|
                       it_values    = it_values ).
  ENDMETHOD.

  METHOD add_softw_comp_filter.
    CHECK it_values IS NOT INITIAL.

    add_devclass_join( iv_ref_table_alias = iv_ref_table_alias
                       iv_ref_field       = iv_ref_field ).

    add_option_filter( iv_fieldname = |{ c_devc_tab_alias }~softwarecomponent|
                       it_values    = it_values ).
  ENDMETHOD.

  METHOD add_package_filter.
    add_option_filter( iv_fieldname = iv_fieldname
                       it_values    = resolve_package_hierarchy( it_values = it_values ) ).
  ENDMETHOD.

  METHOD add_filter.
    mt_criteria = VALUE #( BASE mt_criteria
                           ( is_filter ) ).

    IF is_filter-sign = zif_sat_c_options=>excluding.
      mf_excluding_found = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_group_by_clauses.
    LOOP AT mt_fields_for_grouping INTO DATA(lv_grouping_field).
      add_group_by_clause( lv_grouping_field ).
    ENDLOOP.
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

  METHOD contains_single_i_eq_filter.
    IF lines( it_values ) <> 1.
      RETURN.
    ENDIF.
    DATA(ls_filter) = it_values[ 1 ].
    IF ls_filter-sign = 'I' AND ls_filter-option = 'EQ'.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_date_filter.
    TYPES ty_date_range TYPE RANGE OF dats.
    DATA ls_date_range TYPE LINE OF ty_date_range.

    mo_logger->add_filter_count( 1 ).
    mo_logger->add_filter_value_count( lines( it_values ) ).

    LOOP AT it_values INTO DATA(ls_value).
      ls_date_range = ls_value-low.
      mt_criteria = VALUE #( BASE mt_criteria
                             ( sqlfieldname = iv_fieldname
                               sign         = ls_date_range-sign
                               option       = ls_date_range-option
                               low          = ls_date_range-low
                               high         = ls_date_range-high  ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_option_filter.
    mo_logger->add_filter_count( 1 ).
    mo_logger->add_filter_value_count( lines( it_values ) ).

    mt_criteria = VALUE #( BASE mt_criteria
                           ( LINES OF VALUE #( FOR value IN it_values
                                               ( sqlfieldname = iv_fieldname
                                                 sql_function = iv_sql_function
                                                 sign         = value-sign
                                                 option       = value-option
                                                 low          = value-low
                                                 high         = value-high ) ) ) ).

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
    DATA lt_search_terms TYPE zif_sat_ty_global=>ty_t_string_range.

    IF it_search_terms IS NOT INITIAL.
      lt_search_terms = it_search_terms.
    ELSE.
      lt_search_terms = VALUE #( mo_search_query->mt_search_term[ target = iv_target ]-values OPTIONAL ).
    ENDIF.

    IF lt_search_terms IS INITIAL.
      RETURN.
    ENDIF.

    " If at least one search term with 'E'(Excluding) sign exists all search terms are connected
    " via AND in the SQL Clause
    IF line_exists( lt_search_terms[ sign = zif_sat_c_options=>excluding ] ).
      DATA(lf_use_and_between_terms) = abap_true.
    ENDIF.

    new_and_cond_list( ).

    mo_logger->add_filter_value_count( ( lines( lt_search_terms ) * lines( it_fields ) ) ).
    mo_logger->add_filter_count( lines( it_fields ) ).

    LOOP AT lt_search_terms ASSIGNING FIELD-SYMBOL(<ls_term>).
      DATA(lv_tabix) = sy-tabix.
      DATA(lf_and) = xsdbool( <ls_term>-sign = zif_sat_c_options=>excluding ).

      LOOP AT it_fields INTO DATA(ls_field).

        " if null values are possible for a field we do not want to exclude the rows with null
        IF ls_field-null_possible = abap_true AND lf_and = abap_true.
          new_or_cond_list( ).
          add_filter( VALUE #( sqlfieldname = ls_field-fieldname
                               option       = zif_sat_c_options=>is_null
                               sign         = zif_sat_c_options=>including ) ).
          new_or_cond_list( ).
          add_filter( VALUE #( sqlfieldname = ls_field-fieldname
                               option       = <ls_term>-option
                               sign         = <ls_term>-sign
                               low          = <ls_term>-low  ) ).
          add_filter( VALUE #( sqlfieldname = ls_field-fieldname
                               option       = zif_sat_c_options=>is_not_null
                               sign         = zif_sat_c_options=>including ) ).
        ELSE.
          add_filter( VALUE #( sqlfieldname = ls_field-fieldname
                               option       = <ls_term>-option
                               sign         = <ls_term>-sign
                               low          = <ls_term>-low  ) ).
        ENDIF.

        CHECK sy-tabix <> lines( it_fields ).
        IF lf_and = abap_true.
          new_and_cond_list( ).
        ELSE.
          new_or_cond_list( ).
        ENDIF.
      ENDLOOP.

      IF lv_tabix <> lines( lt_search_terms ).
        IF lf_use_and_between_terms = abap_true.
          new_and_cond_list( ).
        ELSE.
          new_or_cond_list( ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD add_select_field.
    DATA(lv_sql_fieldname) = COND #( WHEN iv_entity IS NOT INITIAL THEN |{ iv_entity }~| )
                                  && |{ iv_fieldname }|.
    mt_select = VALUE #( BASE mt_select
                         ( lv_sql_fieldname &&
                           COND #( WHEN iv_fieldname_alias IS NOT INITIAL THEN | AS { iv_fieldname_alias }| ) ) ).

    IF if_no_grouping = abap_false.
      mt_fields_for_grouping = VALUE #( BASE mt_fields_for_grouping
                                        ( lv_sql_fieldname ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_subquery_filter.
    mt_criteria = VALUE #( BASE mt_criteria
                           ( sqlfieldname = iv_fieldname
                             sign         = iv_sign
                             option       = iv_option
                             subquery     = iv_subquery ) ).
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
        <lv_order_by> = |{ <lv_order_by> }, |.
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
        <lv_select> = |{ <lv_select> }, |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_where_clause.
    CHECK mt_criteria_and IS NOT INITIAL.

    mt_where = zcl_sat_where_clause_builder=>create_and_condition( it_and_seltab = mt_criteria_and ).
  ENDMETHOD.

  METHOD is_grouping_required.
    result = mf_grouping_required.
  ENDMETHOD.

  METHOD add_having_clauses.
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

    SORT lt_texts BY obj_name
                     object.
    DELETE ADJACENT DUPLICATES FROM lt_texts COMPARING obj_name object.

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      EXPORTING clear_buffer = abap_true
      TABLES    obj_tab      = lt_texts.

    LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<ls_entity>).
      ASSIGN lt_texts[ obj_name = <ls_entity>-object_name ] TO FIELD-SYMBOL(<ls_text>).
      IF sy-subrc = 0.
        <ls_entity>-description = <ls_text>-stext.
      ENDIF.
    ENDLOOP.
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

  METHOD new_cond_list.
    IF if_and = abap_true.
      new_and_cond_list( ).
    ELSE.
      new_or_cond_list( ).
    ENDIF.
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

  METHOD reset.
    CLEAR: mt_criteria,
           mt_criteria_or,
           mt_criteria_and,
           mt_where,
           mt_select,
           mt_order_by,
           mt_group_by,
           mt_fields_for_grouping,
           mt_having,
           mt_from,
           ms_join_def,
           mf_excluding_found,
           mf_filters_active,
           mf_distinct_required,
           mf_grouping_required,
           mf_devclass_join_added.
  ENDMETHOD.

  METHOD set_distinct_required.
    mf_distinct_required = abap_true.
  ENDMETHOD.

  METHOD set_filters_active.
    mf_filters_active = abap_true.
  ENDMETHOD.

  METHOD get_select_string.
    add_select_part(
      EXPORTING
        iv_part_name = |SELECT| &&
                       COND #( WHEN mf_distinct_required = abap_true AND mt_group_by IS INITIAL THEN ` DISTINCT` )
        it_part      = mt_select
      CHANGING
        cv_select    = rv_result ).
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

  METHOD add_select_part.
    FIELD-SYMBOLS <lv_part> TYPE any.

    CHECK it_part IS NOT INITIAL.

    cv_select = |{ cv_select }{ c_cr_lf }{ iv_part_name } |.
    LOOP AT it_part ASSIGNING <lv_part>.
      cv_select = |{ cv_select }{ c_cr_lf }    { <lv_part> }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD search_possible.
    result = abap_true.
    IF ms_join_def-primary_table IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    IF     mt_criteria       IS INITIAL
       AND mt_criteria_and   IS INITIAL
       AND mt_criteria_or    IS INITIAL
       AND mf_filters_active  = abap_false.
      result = abap_false.
      RETURN.
    ENDIF.

    IF mt_select IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD add_devclass_join.
    IF mf_devclass_join_added = abap_false.
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_developmentpackage }|
                      iv_alias      = c_devc_tab_alias
                      it_conditions = VALUE #( ( field           = 'developmentpackage'
                                                 ref_field       = iv_ref_field
                                                 ref_table_alias = iv_ref_table_alias
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).
      mf_devclass_join_added = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_package_hierarchy.
    result = VALUE #( FOR range IN it_values
                      WHERE ( option = zif_sat_c_options=>contains_pattern )
                      ( range ) ).

    LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_value_range>) WHERE option <> zif_sat_c_options=>contains_pattern.
      cl_pak_package_queries=>get_all_subpackages( EXPORTING  im_package             = CONV #( <ls_value_range>-low )
                                                              im_also_local_packages = abap_true
                                                   IMPORTING  et_subpackages         = DATA(lt_subpackages)
                                                   EXCEPTIONS OTHERS                 = 1 ).

      result = VALUE #( BASE result
                        ( sign   = <ls_value_range>-sign
                          option = zif_sat_c_options=>equals
                          low    = <ls_value_range>-low )
                        ( LINES OF VALUE #( FOR subpackage IN lt_subpackages
                                            ( sign   = <ls_value_range>-sign
                                              option = zif_sat_c_options=>equals
                                              low    = subpackage-package ) ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD log_success.
    mo_logger->stop_timer( ).
    mo_logger->write_log( ).
  ENDMETHOD.

  METHOD log_select_error.
    mo_logger->stop_timer( ).
    mo_logger->set_select_stmnt( get_select_string( ) ).
    mo_logger->set_error( iv_error ).
    mo_logger->write_log( ).
  ENDMETHOD.

  METHOD log_select_success.
    mo_logger->stop_timer( ).
    mo_logger->set_selected_entries( lines( mt_result ) ).
    mo_logger->set_select_stmnt( get_select_string( ) ).
  ENDMETHOD.

  METHOD log_sql_info.
    mo_logger->set_base_table( ms_join_def-primary_table ).
    mo_logger->set_query_hash( io_query           = mo_search_query
                               is_search_settings = ms_search_engine_params ).
    mo_logger->set_join_count( lines( ms_join_def-tables ) ).
    mo_logger->set_distinct_active( xsdbool( mf_distinct_required = abap_true AND mt_group_by IS INITIAL ) ).
    mo_logger->set_group_by_active( xsdbool( mt_group_by IS NOT INITIAL ) ).
  ENDMETHOD.
ENDCLASS.
