*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_api_state_filter_helper DEFINITION DEFERRED.
CLASS zcl_sat_base_search_provider DEFINITION
      LOCAL FRIENDS lcl_api_state_filter_helper.

CLASS lcl_api_state_filter_helper DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_search_provider TYPE REF TO zcl_sat_base_search_provider
        it_values          TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_ref_field       TYPE fieldname
        iv_ref_table_alias TYPE string
        it_tadir_type      TYPE trobjtype_tab.

    METHODS create_api_filter.

    METHODS has_including_filters
      RETURNING
        VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_fields,
        api_state    TYPE string VALUE 'apistate',
        filter_value TYPE string VALUE 'filtervalue',
        object_name  TYPE string VALUE 'objectname',
        object_type  TYPE string VALUE 'objecttype',
      END OF c_fields.

    CONSTANTS c_api_alias TYPE string VALUE 'api'.

    DATA mo_search_provider TYPE REF TO zcl_sat_base_search_provider.
    DATA mt_values TYPE zif_sat_ty_object_search=>ty_t_value_range.
    DATA mv_ref_field TYPE fieldname.
    DATA mv_ref_table_alias TYPE string.
    DATA mt_tadir_type TYPE trobjtype_tab.
    DATA mv_api_state_subquery TYPE string.

    DATA mt_i_value_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.
    DATA mt_e_value_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.
    DATA mt_i_state_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.
    DATA mt_e_state_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS separate_filters.
    METHODS build_join.
    METHODS add_having_clauses.
    METHODS add_excluding_filters.

    METHODS build_join_conditions
      RETURNING
        VALUE(result) TYPE zsat_join_condition_data_t.

    METHODS add_filters.

    METHODS get_subquery_or_seltab
      IMPORTING
        iv_fieldname  TYPE string
        it_values     TYPE zif_sat_ty_object_search=>ty_t_value_range
      RETURNING
        VALUE(result) TYPE zif_sat_ty_global=>ty_t_or_seltab_sql.

ENDCLASS.
