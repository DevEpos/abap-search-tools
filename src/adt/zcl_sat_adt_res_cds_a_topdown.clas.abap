"! <p class="shorttext synchronized" lang="en">Resource for CDS Top Down Analysis</p>
CLASS zcl_sat_adt_res_cds_a_topdown DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_cds_analysis
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_internal
        REDEFINITION.
    METHODS get_parameters
        REDEFINITION.
  PRIVATE SECTION.
    DATA mf_with_associations TYPE abap_bool.
ENDCLASS.



CLASS ZCL_SAT_ADT_RES_CDS_A_TOPDOWN IMPLEMENTATION.


  METHOD get_internal.
    DATA(lo_cds_parser) = NEW zcl_sat_adt_cds_parser( iv_cds = mv_cds_view ).
    lo_cds_parser->parse_cds(
        if_select_part  = abap_true
        if_associations = mf_with_associations
    ).
    ms_result = lo_cds_parser->ms_select_element_info.
  ENDMETHOD.


  METHOD get_parameters.
    super->get_parameters( io_request ).
    mf_with_associations = zcl_sat_adt_res_util=>get_request_param_value(
      io_request    = io_request
      iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-with_associations
    ).
  ENDMETHOD.
ENDCLASS.
