*"* use this source file for your ABAP unit test classes
CLASS lcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_adt_cds_parser.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Simple parser test</p>
      test_parse1 FOR TESTING RAISING cx_static_check,
      "! <p class="shorttext synchronized" lang="en">Test association retrieval</p>
      test_parse2 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_abap_unit IMPLEMENTATION.

  METHOD test_parse1.
    mr_cut = NEW #( 'SEPMRA_C_PD_PRODUCT' ).
    mr_cut->parse_cds( ).

    DATA(ls_parser_info) = mr_cut->ms_result.
    cl_abap_unit_assert=>assert_not_initial( ls_parser_info-entries ).
  ENDMETHOD.

  METHOD test_parse2.
    FIELD-SYMBOLS: <lt_elements> TYPE zsat_adt_element_info_t.

    mr_cut = NEW #( 'SEPMRA_C_PD_PRODUCT' ).
    mr_cut->parse_cds( if_associations = abap_true ).

    DATA(ls_parser_info) = mr_cut->ms_result.
    cl_abap_unit_assert=>assert_not_initial( ls_parser_info-entries ).

    DATA(ls_associations_entry) = VALUE #( ls_parser_info-entries[ entry_type = zcl_sat_adt_cds_parser=>c_node_type-associations ] OPTIONAL ).
    cl_abap_unit_assert=>assert_not_initial( act = ls_associations_entry ).

  ENDMETHOD.

ENDCLASS.
