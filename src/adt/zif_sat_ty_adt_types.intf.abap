"! <p class="shorttext synchronized">Type definitions for ADT plugin</p>
INTERFACE zif_sat_ty_adt_types
  PUBLIC.

  TYPES:
    " TODO: move to interface ty_global or ty_object_search
    BEGIN OF ty_s_property,
      key   TYPE string,
      type  TYPE string,
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
      count   TYPE i,
      objects TYPE ty_t_adt_obj_ref,
    END OF ty_s_search_result,

    "! Where Used information for a CDS view field
    BEGIN OF ty_s_field_usage,
      entityid      TYPE ddstrucobjname,
      rawentityid   TYPE ddstrucobjname,
      fieldname     TYPE fieldname,
      ddlname       TYPE ddlname,
      sourcetype    TYPE char1,
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
      no_row_limit             TYPE abap_bool,
      combine_filters_with_and TYPE abap_bool,
      fields                   TYPE ty_t_query_input_field,
      custom_options           TYPE ty_t_property,
    END OF ty_s_query_input,

    BEGIN OF ty_cds_top_down_entry,
      entry_type      TYPE string,
      ddl_source_type TYPE string,
      sql_relation    TYPE string,
      alias           TYPE string,
      children        TYPE REF TO data,
      entity_ref      TYPE ty_s_adt_obj_ref,
    END OF ty_cds_top_down_entry,

    ty_cds_top_down_entries TYPE STANDARD TABLE OF ty_cds_top_down_entry WITH EMPTY KEY,

    BEGIN OF ty_where_used_in_cds,
      uri         TYPE string,
      entity_name TYPE string,
      ddlname     TYPE string,
      description TYPE string,
      type        TYPE string,
      source_type TYPE string,
      children    TYPE REF TO data,
    END OF ty_where_used_in_cds,

    ty_where_used_in_cds_t TYPE STANDARD TABLE OF ty_where_used_in_cds WITH EMPTY KEY,

    BEGIN OF ty_cds_top_down_result,
      source_entity TYPE ty_s_adt_obj_ref,
      entries       TYPE ty_cds_top_down_entries,
    END OF ty_cds_top_down_result,

    BEGIN OF ty_cds_ent_usage_info,
      occurrence   TYPE i,
      entity_count TYPE i,
      join_count   TYPE i,
      union_count  TYPE i,
    END OF ty_cds_ent_usage_info,

    BEGIN OF ty_cds_used_entity,
      entity_ref TYPE ty_s_adt_obj_ref,
      usage_info TYPE ty_cds_ent_usage_info,
    END OF ty_cds_used_entity,

    ty_cds_used_entities TYPE STANDARD TABLE OF ty_cds_used_entity WITH EMPTY KEY,

    BEGIN OF ty_cds_used_entities_result,
      source_entity TYPE ty_s_adt_obj_ref,
      used_entities TYPE ty_cds_used_entities,
    END OF ty_cds_used_entities_result,

    BEGIN OF ty_entity_field_info,
      field           TYPE string,
      entity_name     TYPE string,
      alt_entity_name TYPE string,
      type            TYPE string,
      uri             TYPE string,
      description     TYPE string,
      source_type     TYPE string,
      is_key          TYPE abap_bool,
      is_calculated   TYPE abap_bool,
      children        TYPE REF TO data,
    END OF ty_entity_field_info,

    ty_entity_field_infos TYPE STANDARD TABLE OF ty_entity_field_info WITH EMPTY KEY,

    BEGIN OF ty_entity_field_info_result,
      source_field TYPE ty_entity_field_info,
      field_infos  TYPE ty_entity_field_infos,
    END OF ty_entity_field_info_result.
ENDINTERFACE.
