*"* use this source file for your ABAP unit test classes
CLASS lcl_abap_unit DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_get_instance         FOR TESTING RAISING cx_static_check.
    METHODS test_get_instance_with_di FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_abap_unit IMPLEMENTATION.
  METHOD test_get_instance.
    DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                                             iv_contract = 'zif_sat_search_engine' ) ).

    cl_abap_unit_assert=>assert_bound( act = lo_search_engine ).
  ENDMETHOD.

  METHOD test_get_instance_with_di.
    DATA(lo_query_parser) = CAST zif_sat_object_query_parser( zcl_sat_ioc_lookup=>get_instance(
                                                                  iv_contract = 'zif_sat_object_query_parser'
                                                                  iv_filter   = |{ zif_sat_c_object_search=>c_search_type-cds_view }| ) ).
    cl_abap_unit_assert=>assert_bound( act = lo_query_parser ).

    zcl_sat_ioc_lookup=>cleanup( ).
  ENDMETHOD.
ENDCLASS.
