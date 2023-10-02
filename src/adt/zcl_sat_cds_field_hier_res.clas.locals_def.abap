*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_field DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES ty_t_fields TYPE STANDARD TABLE OF REF TO lcl_field WITH DEFAULT KEY.

    DATA view_name TYPE string.
    DATA view_raw_name TYPE string.
    DATA secondary_entity TYPE string.
    DATA field TYPE string.
    DATA raw_field TYPE string.
    DATA source_type TYPE char1.
    DATA is_calculated TYPE abap_bool.
    DATA adt_type TYPE string.
    DATA uri TYPE string.
    DATA parent TYPE REF TO lcl_field.
    DATA children TYPE ty_t_fields.
ENDCLASS.
