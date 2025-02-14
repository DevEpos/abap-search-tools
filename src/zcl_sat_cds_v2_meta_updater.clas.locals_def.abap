*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES BEGIN OF ty_field.
        INCLUDE TYPE zsatcds2mfield.
TYPES   is_cds_v1 TYPE abap_bool.
TYPES END OF ty_field.

TYPES BEGIN OF ty_basetable.
        INCLUDE TYPE zsatcds2mbtab.
TYPES END OF ty_basetable.

TYPES ty_basetables TYPE SORTED TABLE OF ty_basetable WITH UNIQUE DEFAULT KEY.
TYPES: BEGIN OF ty_literal,
         char TYPE abap_bool,
         num  TYPE abap_bool,
       END OF ty_literal.

TYPES ty_fields TYPE SORTED TABLE OF ty_field WITH UNIQUE KEY ddlname fieldpos.

CLASS lcl_parser DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ddls TYPE ddddlsrc.

    METHODS parse
      EXPORTING
        !fields    TYPE ty_fields
        basetables TYPE ty_basetables.

  PRIVATE SECTION.
    DATA ddls TYPE ddddlsrc.
ENDCLASS.


CLASS lcl_field_visitor DEFINITION
  INHERITING FROM cl_qlast_visitor.

  PUBLIC SECTION.
    DATA current_field TYPE string.

    METHODS constructor
      IMPORTING
        source_entityname TYPE ddddlsrc-ddlname.

    METHODS visit_stdselectlist_entry REDEFINITION.
    METHODS visit_atomic_expression   REDEFINITION.
    METHODS visit_literal_expression  REDEFINITION.

    METHODS get_found_fields
      RETURNING
        VALUE(result) TYPE ty_fields.

    METHODS if_qlast_visitor~get_descend REDEFINITION.
    METHODS if_qlast_visitor~after       REDEFINITION.
    METHODS if_qlast_visitor~ignore      REDEFINITION.

  PRIVATE SECTION.
    DATA field_pos TYPE mcpos VALUE 1.
    DATA found_fields TYPE ty_fields.
    DATA source_field TYPE string.
    DATA literal_found TYPE ty_literal.
    DATA atomic_field_found TYPE abap_bool.
    DATA source_entityname TYPE string.
    DATA ignore_flags TYPE if_qlast_visitor=>bitmask.
ENDCLASS.
