*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mo_in_converter  TYPE REF TO cl_abap_conv_in_ce.
    DATA mo_out_converter TYPE REF TO cl_abap_conv_out_ce.

    METHODS convert_response_body
      IMPORTING
        iv_message_body TYPE xstring
      RETURNING
        VALUE(result)   TYPE zif_sat_ty_adt_types=>ty_s_search_result.

    methods run_test
    importing
    iv_uri_part type string
    returning
      value(result) type zif_sat_ty_adt_types=>ty_s_search_result.

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
                                                     zif_sat_c_object_search=>c_cds_search_params-select_from ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_search_result-objects ) ).
  ENDMETHOD.

  METHOD test_method2.
    DATA(ls_search_result) = run_test(
        iv_uri_part = '?entityName=zsat_p_cdsviewbase&sourceOrigin=' &&
                       zif_sat_c_object_search=>c_cds_search_params-select_from &&
                       '&' && zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_released_entities && '=true' ).

    cl_abap_unit_assert=>assert_initial( ls_search_result-objects ).
  ENDMETHOD.

  METHOD test_method3.
    DATA(ls_search_result) = run_test(
        iv_uri_part = '?entityName=zsat_i_apistates&sourceOrigin=' &&
                      zif_sat_c_object_search=>c_cds_search_params-association &&
                      '&' && zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_local_assocs && '=true' ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( ls_search_result-objects ) ).
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
        CALL TRANSFORMATION zsat_search_result
          SOURCE XML lv_xml
         RESULT object_search_result = result.
      CATCH cx_root INTO DATA(lx_error). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
