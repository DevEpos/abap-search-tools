"! <p class="shorttext synchronized" lang="en">Type definitions for ADT plugin</p>
INTERFACE zif_sat_ty_adt_types
  PUBLIC .

  TYPES:
    "! Where Used information for a CDS view field
    BEGIN OF ty_s_field_usage,
      entityid      TYPE ddstrucobjname,
      rawentityid   TYPE ddstrucobjname,
      fieldname     TYPE fieldname,
      ddlname       TYPE ddlname,
      sourcetype    TYPE ddddlsrctype,
      apistate      TYPE string,
      is_calculated TYPE abap_bool,
    END OF ty_s_field_usage.

  TYPES: ty_t_field_usage TYPE STANDARD TABLE OF ty_s_field_usage WITH EMPTY KEY.
ENDINTERFACE.
