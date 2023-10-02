*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION DEFERRED.
CLASS zcl_sat_adt_res_cds_a_wusl DEFINITION
  LOCAL FRIENDS ltcl_abap_unit.
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mo_in_converter TYPE REF TO cl_abap_conv_in_ce.
    DATA mo_out_converter TYPE REF TO cl_abap_conv_out_ce.

    METHODS convert_response_body
      IMPORTING
        iv_message_body TYPE xstring
      RETURNING
        VALUE(result)   TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds_t.

    METHODS run_test
      IMPORTING
        iv_uri_part   TYPE string
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds_t.

    METHODS test_method1 FOR TESTING.
    METHODS test_method2 FOR TESTING.
    METHODS test_method3 FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD constructor.
    mo_in_converter = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    mo_out_converter = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  ENDMETHOD.

  METHOD test_method1.
    DATA(ls_search_result) = run_test( iv_uri_part = '?entityName=zsat_p_cdsviewbase&sourceOrigin=' &&
                                                     zcl_sat_adt_res_cds_a_wusl=>c_source_origin-select_from ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_search_result ) ).
  ENDMETHOD.

  METHOD test_method2.
    DATA(lt_wusl_result) = run_test(
        iv_uri_part = '?entityName=zsat_p_cdsviewbase&sourceOrigin=' &&
                       zcl_sat_adt_res_cds_a_wusl=>c_source_origin-select_from &&
                       '&' && zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_released_entities && '=true' ).

    cl_abap_unit_assert=>assert_initial( lt_wusl_result ).
  ENDMETHOD.

  METHOD test_method3.
    DATA(lt_wusl_result) = run_test(
        iv_uri_part = '?entityName=zsat_i_apistates&sourceOrigin=' &&
                      zcl_sat_adt_res_cds_a_wusl=>c_source_origin-association &&
                      '&' && zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_local_assocs && '=true' ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_wusl_result ) ).
  ENDMETHOD.

  METHOD run_test.
    DATA ls_response TYPE sadt_rest_response.

    DATA(ls_request) = VALUE sadt_rest_request(
                                 request_line = VALUE #(
                                     method = 'GET'
                                     uri    = '/devepos/adt/saat/cds/analysis/whereUsedIn' && iv_uri_part ) ).

    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING request  = ls_request
      IMPORTING response = ls_response.

    cl_abap_unit_assert=>assert_subrc( ).

    IF ls_response-message_body IS NOT INITIAL.
      result = convert_response_body( ls_response-message_body ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_response_body.
    DATA lv_xml TYPE string.

    mo_in_converter->convert( EXPORTING input = iv_message_body
                              IMPORTING data  = lv_xml ).

    TRY.
        CALL TRANSFORMATION zsat_where_used_in_cds_result
             SOURCE XML lv_xml
             RESULT wusl_result = result.
      CATCH cx_root INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_error ).
  ENDMETHOD.
ENDCLASS.
