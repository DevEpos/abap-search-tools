*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION DEFERRED.
CLASS zcl_sat_cds_wusi_analysis DEFINITION
  LOCAL FRIENDS ltcl_abap_unit.
CLASS ltcl_abap_unit DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS run_test
      IMPORTING
        iv_entity           TYPE zsat_entity_id
        iv_source_origin    TYPE string
        if_released_only    TYPE abap_bool
        if_only_local_assoc TYPE abap_bool
      RETURNING
        VALUE(result)       TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds_t.

    METHODS test_method1 FOR TESTING.
    METHODS test_method2 FOR TESTING.
    METHODS test_method3 FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD test_method1.
    DATA(lt_result) = run_test( iv_entity           = 'ZSAT_P_CDSVIEWBASE'
                                iv_source_origin    = zcl_sat_cds_wusi_analysis=>c_source_origin-select_from
                                if_released_only    = abap_false
                                if_only_local_assoc = abap_false ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_result ) ).
  ENDMETHOD.

  METHOD test_method2.
    DATA(lt_result) = run_test( iv_entity           = 'ZSAT_P_CDSVIEWBASE'
                                iv_source_origin    = zcl_sat_cds_wusi_analysis=>c_source_origin-select_from
                                if_released_only    = abap_true
                                if_only_local_assoc = abap_false ).

    cl_abap_unit_assert=>assert_initial( lt_result ).
  ENDMETHOD.

  METHOD test_method3.
    DATA(lt_result) = run_test( iv_entity           = 'ZSAT_I_APISTATES'
                                iv_source_origin    = zcl_sat_cds_wusi_analysis=>c_source_origin-association
                                if_released_only    = abap_false
                                if_only_local_assoc = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
  ENDMETHOD.

  METHOD run_test.
    DATA(lo_analyzer) = NEW zcl_sat_cds_wusi_analysis( iv_entity           = iv_entity
                                                       iv_source_origin    = iv_source_origin ).
    TRY.
        lo_analyzer->run( ).
        result = lo_analyzer->get_result( ).
      CATCH zcx_sat_application_exc.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
