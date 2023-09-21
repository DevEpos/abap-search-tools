*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_node DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_cached_node,
        name TYPE string,
        node TYPE REF TO lcl_node,
      END OF ty_s_cached_node,
      ty_t_cached_nodes TYPE STANDARD TABLE OF ty_s_cached_node WITH KEY name.
    TYPES ty_t_nodes TYPE STANDARD TABLE OF REF TO lcl_node WITH DEFAULT KEY.

    DATA name            TYPE string.
    DATA name2           TYPE string.
    DATA type            TYPE string.
    DATA adt_type        TYPE string.
    DATA entity_type     TYPE zsat_entity_type.
    DATA relation        TYPE string.
    DATA alias           TYPE string.
    DATA source_type     TYPE char1.
    DATA entity_name     TYPE string.
    DATA raw_entity_name TYPE string.
    DATA description     TYPE string.
    DATA package         TYPE string.
    DATA owner           TYPE string.
    DATA uri             TYPE string.
    DATA parent          TYPE REF TO lcl_node.
    DATA children        TYPE ty_t_nodes.
    DATA ddls_name       TYPE string.
ENDCLASS.


CLASS lcl_node_helper DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mo_root_node TYPE REF TO lcl_node READ-ONLY.
    DATA mt_cds_views TYPE lcl_node=>ty_t_cached_nodes.
    DATA mt_tables    TYPE lcl_node=>ty_t_cached_nodes.
    DATA mt_views     TYPE lcl_node=>ty_t_cached_nodes.

    "! <p class="shorttext synchronized">Creates new element information</p>
    METHODS constructor
      IMPORTING
        iv_name            TYPE string
        iv_entity_name     TYPE string OPTIONAL
        iv_raw_entity_name TYPE string OPTIONAL.

    "! <p class="shorttext synchronized">Adds the given element information as child</p>
    METHODS add_child
      IMPORTING
        io_parent_node     TYPE REF TO lcl_node
        iv_name            TYPE string           OPTIONAL
        iv_type            TYPE string           OPTIONAL
        iv_entity_type     TYPE zsat_entity_type OPTIONAL
        iv_alias           TYPE string           OPTIONAL
        iv_relation        TYPE string           OPTIONAL
        iv_entity_name     TYPE string           OPTIONAL
        iv_raw_entity_name TYPE string           OPTIONAL
      RETURNING
        VALUE(ro_added)    TYPE REF TO lcl_node.

    "! <p class="shorttext synchronized">Sets parent of current node as new parent node</p>
    METHODS set_parent_as_current_node.

    "! <p class="shorttext synchronized">Checks if the current parent element has children</p>
    METHODS current_parent_has_children
      RETURNING
        VALUE(rf_has_children) TYPE abap_bool.

    METHODS get_relation
      IMPORTING
        io_parent_node         TYPE REF TO lcl_node
        iv_datasource_type     TYPE qlast_datasource_type
      RETURNING
        VALUE(rv_sql_relation) TYPE string.

    METHODS convert_to_adt_result
      IMPORTING
        io_node       TYPE REF TO lcl_node
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_cds_top_down_result.

    "! <p class="shorttext synchronized">Converts node into element information</p>
    METHODS convert_node_to_top_down_ent
      IMPORTING
        io_node           TYPE REF TO lcl_node
      CHANGING
        cs_top_down_entry TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entry.

  PRIVATE SECTION.
    DATA mo_current_node TYPE REF TO lcl_node.

    METHODS fill_element_info_from_node
      IMPORTING
        io_node           TYPE REF TO lcl_node
      CHANGING
        cs_top_down_entry TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entry.
ENDCLASS.


CLASS lcl_ddl_stmnt_interpreter DEFINITION ABSTRACT.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Interpret the DDL statement</p>
    METHODS interpret ABSTRACT.

    METHODS constructor
      IMPORTING
        io_node_helper TYPE REF TO lcl_node_helper.

  PROTECTED SECTION.
    DATA mo_node_helper TYPE REF TO lcl_node_helper.
ENDCLASS.

CLASS lcl_ddl_view_stmnt_intrpt DEFINITION
  INHERITING FROM lcl_ddl_stmnt_interpreter.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        if_associations TYPE abap_bool OPTIONAL
        io_node_helper  TYPE REF TO lcl_node_helper
        io_stmnt        TYPE REF TO cl_qlast_view_definition.

    METHODS interpret REDEFINITION.

  PRIVATE SECTION.
    DATA mo_stmnt        TYPE REF TO cl_qlast_view_definition.
    DATA mf_associations TYPE abap_bool.

    "! <p class="shorttext synchronized">Interpret Select statement</p>
    METHODS interpret_select_stmnt
      IMPORTING
        io_parent_node TYPE REF TO lcl_node
        io_select      TYPE REF TO cl_qlast_select
        if_union       TYPE abap_bool OPTIONAL
        if_union_all   TYPE abap_bool OPTIONAL.

    "! <p class="shorttext synchronized">Interpret Join data source</p>
    METHODS interpret_join
      IMPORTING
        io_parent_node     TYPE REF TO lcl_node
        io_join_datasource TYPE REF TO cl_qlast_join_datasource.

    "! <p class="shorttext synchronized">Interpret data source</p>
    METHODS interpret_datasource
      IMPORTING
        io_parent_node TYPE REF TO lcl_node
        io_datasource  TYPE REF TO cl_qlast_datasource
        iv_parent_type TYPE qlast_datasource_type.

    "! <p class="shorttext synchronized">Get associations of data source</p>
    METHODS get_associations
      IMPORTING
        io_parent_node TYPE REF TO lcl_node
        io_select      TYPE REF TO cl_qlast_select.
ENDCLASS.
