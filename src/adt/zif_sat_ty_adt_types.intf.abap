"! <p class="shorttext synchronized">Type definitions for ADT plugin</p>
INTERFACE zif_sat_ty_adt_types
  PUBLIC.

  TYPES:
    BEGIN OF ty_s_property,
      key   TYPE string,
      value TYPE string,
    END OF ty_s_property,

    ty_t_property TYPE SORTED TABLE OF ty_s_property WITH UNIQUE KEY key,

    BEGIN OF ty_s_adt_obj_ref,
      uri         TYPE string,
      parent_uri  TYPE string,
      name        TYPE string,
      alt_name    TYPE string,
      description TYPE string,
      type        TYPE string,
      devclass    TYPE string,
      owner       TYPE string,
      created_on  TYPE dats,
      changed_by  TYPE uname,
      changed_on  TYPE dats,
      properties  TYPE ty_t_property,
    END OF ty_s_adt_obj_ref,

    ty_t_adt_obj_ref TYPE STANDARD TABLE OF ty_s_adt_obj_ref WITH EMPTY KEY,

    BEGIN OF ty_s_search_result,
      objects TYPE ty_t_adt_obj_ref,
    END OF ty_s_search_result,

    "! Where Used information for a CDS view field
    BEGIN OF ty_s_field_usage,
      entityid      TYPE ddstrucobjname,
      rawentityid   TYPE ddstrucobjname,
      fieldname     TYPE fieldname,
      ddlname       TYPE ddlname,
      sourcetype    TYPE char1,
      apistate      TYPE string,
      is_calculated TYPE abap_bool,
    END OF ty_s_field_usage,

    ty_t_field_usage TYPE STANDARD TABLE OF ty_s_field_usage WITH EMPTY KEY,

    BEGIN OF ty_s_query_input_filter,
      name   TYPE string,
      values TYPE string_table,
    END OF ty_s_query_input_filter,

    ty_t_query_input_filter TYPE STANDARD TABLE OF ty_s_query_input_filter WITH EMPTY KEY,

    BEGIN OF ty_s_query_input_field,
      name    TYPE string,
      values  TYPE string_table,
      filters TYPE ty_t_query_input_filter,
    END OF ty_s_query_input_field,

    ty_t_query_input_field TYPE STANDARD TABLE OF ty_s_query_input_field WITH EMPTY KEY,

    BEGIN OF ty_s_query_input,
      type                     TYPE zif_sat_ty_object_search=>ty_search_type,
      max_rows                 TYPE i,
      combine_filters_with_and TYPE abap_bool,
      with_api_state           TYPE abap_bool,
      fields                   TYPE ty_t_query_input_field,
    END OF ty_s_query_input.
ENDINTERFACE.
