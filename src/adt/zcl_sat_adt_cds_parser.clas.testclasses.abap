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
    mr_cut = NEW #( 'I_PRODUCT' ).
    mr_cut->parse_cds( ).

    DATA(ls_parser_info) = mr_cut->ms_select_element_info.
    cl_abap_unit_assert=>assert_not_initial( ls_parser_info-children ).
  ENDMETHOD.

  METHOD test_parse2.
    FIELD-SYMBOLS: <lt_elements> TYPE zsat_adt_element_info_t.

    mr_cut = NEW #( 'I_PRODUCT' ).
    mr_cut->parse_cds( if_associations = abap_true ).

    DATA(ls_parser_info) = mr_cut->ms_select_element_info.
    cl_abap_unit_assert=>assert_not_initial( ls_parser_info-children ).

    ASSIGN ls_parser_info-children->* TO <lt_elements>.
    cl_abap_unit_assert=>assert_equals( act = sy-subrc exp = 0 ).

    DATA(ls_associations_entry) = VALUE #( <lt_elements>[ name = zcl_sat_adt_cds_parser=>c_node_type-associations ] OPTIONAL ).
    cl_abap_unit_assert=>assert_not_initial( act = ls_associations_entry ).

  ENDMETHOD.

ENDCLASS.
