*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_field DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      ty_t_fields TYPE STANDARD TABLE OF REF TO lcl_field WITH DEFAULT KEY.
    DATA:
      view_name        TYPE string,
      view_raw_name    TYPE string,
      secondary_entity TYPE string,
      field            TYPE string,
      raw_field        TYPE string,
      source_type      TYPE char1,
      is_calculated    TYPE abap_bool,
      adt_type         TYPE string,
      uri              TYPE string,
      parent           TYPE REF TO lcl_field,
      children         TYPE ty_t_fields.
ENDCLASS.
