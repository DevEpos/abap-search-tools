"! <p class="shorttext synchronized">CDS Analysis for ADT</p>
CLASS zcl_sat_adt_res_cds_analysis DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.
    DATA mv_cds_view TYPE zsat_cds_view_name.
    DATA ms_result   TYPE zsat_adt_element_info.

    "! <p class="shorttext synchronized">Retrieve parameters from ADT REST request</p>
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Executes the GET request</p>
    METHODS get_internal ABSTRACT
      RAISING
        cx_adt_rest.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_analysis IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).
    get_internal( ).

    IF ms_result IS INITIAL.
      RETURN.
    ENDIF.

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_generic_eleminfo_res_ch( )
                             data            = ms_result ).
  ENDMETHOD.

  METHOD get_parameters.
    mv_cds_view = zcl_sat_adt_res_util=>get_request_param_value(
                      io_request    = io_request
                      iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-cds_name
                      if_mandatory  = abap_true ).
  ENDMETHOD.
ENDCLASS.
