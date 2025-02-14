"! <p class="shorttext synchronized">Where-Used-Analysis for Table/View/CDS in CDS Views</p>
CLASS zcl_sat_adt_res_cds_a_wusl DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_result TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds_t.
    DATA mv_entity TYPE zsat_entity_id.
    DATA mf_only_local_assocs TYPE abap_bool.
    DATA mv_source_origin TYPE string.
    DATA mf_released_entitites_only TYPE abap_bool.
    DATA mf_recursive_search TYPE abap_bool.

    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

    METHODS execute_where_used_in_cds
      RAISING
        cx_adt_rest.

    METHODS fill_response
      IMPORTING
        io_response TYPE REF TO if_adt_rest_response
      RAISING
        cx_adt_rest.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_a_wusl IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).
    execute_where_used_in_cds( ).
    fill_response( io_response = response ).
  ENDMETHOD.

  METHOD get_parameters.
    mv_entity = to_upper( zcl_sat_adt_res_util=>get_request_param_value(
                              io_request    = io_request
                              iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-entity_name
                              if_mandatory  = abap_true ) ).
    mv_source_origin = zcl_sat_adt_res_util=>get_request_param_value(
                           io_request    = io_request
                           iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-source_origin
                           if_mandatory  = abap_true ).
    mf_recursive_search = zcl_sat_adt_res_util=>get_boolean_req_param(
                              iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-recursive_search
                              io_request    = io_request ).
    mf_released_entitites_only = zcl_sat_adt_res_util=>get_boolean_req_param(
        iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_released_entities
        io_request    = io_request ).
    mf_only_local_assocs = zcl_sat_adt_res_util=>get_boolean_req_param(
                               iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_local_assocs
                               io_request    = io_request ).
  ENDMETHOD.

  METHOD execute_where_used_in_cds.
    DATA(lo_cds_where_used_analysis) = NEW zcl_sat_cds_wusi_analysis(
                                               iv_entity           = mv_entity
                                               iv_source_origin    = mv_source_origin
                                               if_recursive        = mf_recursive_search
                                               if_only_local_assoc = mf_only_local_assocs
                                               if_only_released    = mf_released_entitites_only ).
    TRY.
        lo_cds_where_used_analysis->run( ).
        mt_result = lo_cds_where_used_analysis->get_result( ).
      CATCH zcx_sat_application_exc INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_rest_error
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text(
                                 exception = COND #( WHEN lx_error->previous IS BOUND
                                                     THEN lx_error->previous
                                                     ELSE lx_error ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD fill_response.
    io_response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_where_used_in_cds_res_h( )
        data            = CORRESPONDING zif_sat_ty_adt_types=>ty_where_used_in_cds_t(  mt_result ) ).
  ENDMETHOD.
ENDCLASS.
