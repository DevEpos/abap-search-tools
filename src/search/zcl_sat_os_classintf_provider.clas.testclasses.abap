*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_os_classintf_provider.

    METHODS:
      search_with_terms_only FOR TESTING,
      search_description FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD search_with_terms_only.
    mr_cut = NEW #( ).

    DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance( iv_contract = 'zif_sat_search_engine' ) ).
    TRY.
        DATA(lo_query) = lo_search_engine->create_query(
           iv_search_terms = 'cl_ris_adt_res_app<'
           iv_search_type  = zif_sat_c_object_search=>c_search_type-class_interface
        ).

        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
            msg  = 'More than one result'
            quit = if_aunit_constants=>quit-test
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
        msg  = 'Error during query execution'
        quit = if_aunit_constants=>quit-test
    ).
  ENDMETHOD.

  METHOD search_description.
    mr_cut = NEW #( ).

    DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance( iv_contract = 'zif_sat_search_engine' ) ).
    TRY.
        DATA(lo_query) = lo_search_engine->create_query(
           it_options      = VALUE #(
             ( option      = zif_sat_c_object_search=>c_search_option-by_description
               value_range = VALUE #( ( low = 'Self test listener Strictly internal' ) ) )
           )
           iv_search_type  = zif_sat_c_object_search=>c_search_type-class_interface
        ).
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
            msg  = 'More than one result'
            quit = if_aunit_constants=>quit-test
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
        msg  = 'Error during query execution'
        quit = if_aunit_constants=>quit-test
    ).
  ENDMETHOD.

ENDCLASS.
