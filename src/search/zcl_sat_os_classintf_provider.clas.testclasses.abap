*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_os_classintf_provider.

    METHODS:
      search_with_terms_only FOR TESTING,
      search_description for testing.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD search_with_terms_only.
*    TRY.
*        mr_cut = NEW #(
*            ir_query = zcl_sat_object_search_query=>create_query(
*              iv_search_string        = 'cl_ris_adt_res_app<'
*              iv_type                 = zif_sat_c_object_browser_mode=>class_interface
*              is_search_engine_params = VALUE #( use_and_cond_for_options = abap_true )
*              it_options              = value #(
*              )
*            )
*        ).
*        DATA(lt_result) = mr_cut->zif_sat_object_searcher~search( ).
*        cl_abap_unit_assert=>assert_equals(
*            act  = lines( lt_result )
*            exp  = 1
*            msg  = 'More than one result'
*            quit = if_aunit_constants=>quit-test
*        ).
*      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_not_bound(
*        act  = lx_search_error
*        msg  = 'Error during query execution'
*        quit = if_aunit_constants=>quit-test
*    ).
  ENDMETHOD.

  METHOD search_description.
*    TRY.
*        mr_cut = NEW #(
*            ir_query = zcl_sat_object_search_query=>create_query(
*              iv_search_string        = space
*              iv_type                 = zif_sat_c_object_browser_mode=>class_interface
*              is_search_engine_params = VALUE #( use_and_cond_for_options = abap_true )
*              it_options              = value #(
*                ( option      = zif_sat_c_object_search=>c_search_option-by_description
*                  value_range = value #( ( low = 'Self test listener Strictly internal' )  ) )
*              )
*            )
*        ).
*        DATA(lt_result) = mr_cut->zif_sat_object_searcher~search( ).
*        cl_abap_unit_assert=>assert_equals(
*            act  = lines( lt_result )
*            exp  = 1
*            msg  = 'More than one result'
*            quit = if_aunit_constants=>quit-test
*        ).
*      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_not_bound(
*        act  = lx_search_error
*        msg  = 'Error during query execution'
*        quit = if_aunit_constants=>quit-test
*    ).

  ENDMETHOD.

ENDCLASS.
