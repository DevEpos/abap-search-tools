*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES ty_t_fields TYPE SORTED TABLE OF fieldname WITH UNIQUE DEFAULT KEY.

CLASS lcl_field_visitor DEFINITION
  INHERITING FROM cl_qlast_visitor.

  PUBLIC SECTION.
    DATA mv_current_field TYPE string.

    METHODS constructor
      IMPORTING
        iv_source_entityname TYPE string
        iv_source_field      TYPE string.

    METHODS visit_calc
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_atomic_expression REDEFINITION.

    METHODS get_found_fields
      RETURNING
        VALUE(rt_found_fields) TYPE ty_t_fields.

  PRIVATE SECTION.
    DATA mt_found_fields TYPE ty_t_fields.
    DATA mv_source_field TYPE string.
    DATA mv_source_entityname TYPE string.
    DATA mf_calc_field_found TYPE abap_bool.
ENDCLASS.
