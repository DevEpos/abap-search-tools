*"* use this source file for your ABAP unit test classes
CLASS lcl_abap_unit DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_adt_cds_parser.

    "! <p class="shorttext synchronized">Simple parser test</p>
    METHODS test_parse1 FOR TESTING RAISING cx_static_check.
    "! <p class="shorttext synchronized">Test association retrieval</p>
    METHODS test_parse2 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_abap_unit IMPLEMENTATION.
  METHOD test_parse1.
    mr_cut = NEW #( 'SEPMRA_C_PD_PRODUCT' ).
    mr_cut->parse_cds( ).

    DATA(ls_parser_info) = mr_cut->ms_result.
    cl_abap_unit_assert=>assert_not_initial( ls_parser_info-entries ).
  ENDMETHOD.

  METHOD test_parse2.
    mr_cut = NEW #( 'SEPMRA_C_PD_PRODUCT' ).
    mr_cut->parse_cds( if_associations = abap_true ).

    DATA(ls_parser_info) = mr_cut->ms_result.
    cl_abap_unit_assert=>assert_not_initial( ls_parser_info-entries ).

    DATA(ls_associations_entry) = VALUE #( ls_parser_info-entries[
                                               entry_type = zcl_sat_adt_cds_parser=>c_node_type-associations ] OPTIONAL ).
    cl_abap_unit_assert=>assert_not_initial( act = ls_associations_entry ).
  ENDMETHOD.
ENDCLASS.
