*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    DATA cut              TYPE REF TO zcl_sat_adt_res_object_search.
    DATA mo_in_converter  TYPE REF TO cl_abap_conv_in_ce.
    DATA mo_out_converter TYPE REF TO cl_abap_conv_out_ce.

    METHODS convert_request_body
      IMPORTING
        is_query_input TYPE  zif_sat_ty_adt_types=>ty_s_query_input
      RETURNING
        VALUE(result)  TYPE xstring.

    METHODS convert_response_body
      IMPORTING
        iv_message_body TYPE xstring
      RETURNING
        VALUE(result)   TYPE zif_sat_ty_adt_types=>ty_s_search_result.

    METHODS test_method FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD test_method.
    DATA ls_response TYPE sadt_rest_response.

    DATA(ls_input) = VALUE zif_sat_ty_adt_types=>ty_s_query_input(
        type     = zif_sat_c_object_search=>c_search_type-cds_view
        max_rows = 1
        fields   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              values  = VALUE #( ( `zsat_i_associatedincds` ) ) )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              filters = VALUE #( ( name = 'field' values = VALUE #( ( `ddlname` ) ) ) ) ) ) ).

    DATA(ls_request) = VALUE sadt_rest_request(
                                 request_line  = VALUE #( method = 'POST' uri = '/devepos/adt/saat/objectsearch' )
                                 header_fields = VALUE #( ( name = 'Content-Type' value = 'application/xml' ) )
                                 message_body  = convert_request_body( ls_input ) ).

    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      EXPORTING request  = ls_request
      IMPORTING response = ls_response.

    cl_abap_unit_assert=>assert_subrc( ).

    IF ls_response-message_body IS NOT INITIAL.
      DATA(ls_search_result) = convert_response_body( ls_response-message_body ).

      cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( ls_search_result-objects ) ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    mo_in_converter = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    mo_out_converter = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  ENDMETHOD.

  METHOD convert_request_body.
    DATA lv_xml TYPE string.

    CALL TRANSFORMATION zsat_object_search_query_input
      SOURCE query_input = is_query_input
      RESULT XML lv_xml.

    mo_out_converter->convert( EXPORTING data   = lv_xml
                               IMPORTING buffer = result ).
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
        IF 1 = 2.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
