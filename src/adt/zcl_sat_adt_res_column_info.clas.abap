"! <p class="shorttext synchronized" lang="en">Resource for reading information of a Column</p>
CLASS zcl_sat_adt_res_column_info DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get
        REDEFINITION .
  PROTECTED SECTION.
    CONSTANTS c_field_name TYPE string VALUE 'FIELD' ##NO_TEXT.

    DATA mv_field TYPE fieldname.
    DATA mv_object_name TYPE zsat_entity_id.
    DATA mv_mode TYPE string.

    DATA ms_field_info TYPE zsat_adt_element_info.

    "! <p class="shorttext synchronized" lang="en">Retrieves request parameters</p>
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.
    METHODS internal_get ABSTRACT
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_column_info IMPLEMENTATION.


  METHOD get.

    get_parameters( request ).
    internal_get( request ).

    CHECK ms_field_info IS NOT INITIAL.

    response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_generic_eleminfo_res_ch( )
        data            = ms_field_info
    ).
  ENDMETHOD.


  METHOD get_parameters.

    mv_object_name = zcl_sat_adt_res_util=>get_request_param_value(
      iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-name
      if_mandatory     = abap_true
      io_request       = io_request
    ).
    mv_field = zcl_sat_adt_res_util=>get_request_param_value(
      iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-field
      if_mandatory     = abap_true
      io_request       = io_request
    ).

  ENDMETHOD.

ENDCLASS.
