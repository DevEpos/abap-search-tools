"! <p class="shorttext synchronized">Resource for CDS Top Down Analysis</p>
CLASS zcl_sat_adt_res_cds_a_topdown DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_cds_analysis
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.
    METHODS get_parameters REDEFINITION.

  PRIVATE SECTION.
    DATA mf_with_associations TYPE abap_bool.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_a_topdown IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).

    DATA(lo_cds_parser) = NEW zcl_sat_adt_cds_parser( iv_cds = mv_cds_view ).
    lo_cds_parser->parse_cds( if_select_part  = abap_true
                              if_associations = mf_with_associations ).

    IF lo_cds_parser->ms_result-entries IS NOT INITIAL.
      response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_top_down_res_handler( )
                               data            = lo_cds_parser->ms_result ).
    ENDIF.
  ENDMETHOD.

  METHOD get_parameters.
    super->get_parameters( io_request ).
    mf_with_associations = zcl_sat_adt_res_util=>get_request_param_value(
                               io_request    = io_request
                               iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-with_associations ).
  ENDMETHOD.
ENDCLASS.
