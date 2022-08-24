*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES BEGIN OF ty_field.
INCLUDE TYPE zsatcds2mfield.
TYPES is_cds_v1 TYPE abap_bool.
TYPES END OF ty_field.

TYPES BEGIN OF ty_basetable.
INCLUDE TYPE zsatcds2mbtab.
TYPES END OF ty_basetable.

TYPES:
  ty_basetables TYPE SORTED TABLE OF ty_basetable WITH UNIQUE DEFAULT KEY,
  BEGIN OF ty_literal,
    char TYPE abap_bool,
    num  TYPE abap_bool,
  END OF ty_literal,

  ty_fields TYPE SORTED TABLE OF ty_field WITH UNIQUE KEY ddlname fieldpos.

CLASS lcl_parser DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          ddls TYPE ddddlsrc,
      parse
        EXPORTING
          fields     TYPE ty_fields
          basetables TYPE ty_basetables.
  PRIVATE SECTION.
    DATA:
      ddls TYPE ddddlsrc.
ENDCLASS.

CLASS lcl_field_visitor DEFINITION
 INHERITING FROM cl_qlast_visitor.
  PUBLIC SECTION.
    DATA current_field TYPE string.

    METHODS: constructor
      IMPORTING
        source_entityname TYPE ddddlsrc-ddlname,
      visit_stdselectlist_entry
        REDEFINITION,
      visit_atomic_expression
        REDEFINITION,
      visit_literal_expression
        REDEFINITION,
      get_found_fields
        RETURNING
          VALUE(result) TYPE ty_fields,
      if_qlast_visitor~get_descend
        REDEFINITION,
      if_qlast_visitor~after
        REDEFINITION,
      if_qlast_visitor~ignore
        REDEFINITION.
  PRIVATE SECTION.
    DATA:
      field_pos          TYPE mcpos VALUE 1,
      found_fields       TYPE ty_fields,
      source_field       TYPE string,
      literal_found      TYPE ty_literal,
      atomic_field_found TYPE abap_bool,
      source_entityname  TYPE string,
      ignore_flags       TYPE if_qlast_visitor=>bitmask.
ENDCLASS.
