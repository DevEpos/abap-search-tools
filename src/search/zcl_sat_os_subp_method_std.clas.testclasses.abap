*"* use this source file for your ABAP unit test classes
* Mock class for Search Query
CLASS lcl_query DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_query.

    TYPES ty_search_term TYPE RANGE OF string.

    METHODS constructor
      IMPORTING
        it_search_term    TYPE zif_sat_ty_object_search=>ty_t_search_term   OPTIONAL
        iv_type           TYPE zif_sat_ty_object_search=>ty_search_type     OPTIONAL
        iv_max_rows       TYPE sy-tabix                                     DEFAULT 50
        it_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL.
ENDCLASS.


CLASS lcl_query IMPLEMENTATION.
  METHOD constructor.
    zif_sat_object_search_query~mt_search_term = it_search_term.
    zif_sat_object_search_query~mv_type = iv_type.
    zif_sat_object_search_query~mv_max_rows = iv_max_rows.
    zif_sat_object_search_query~mt_search_options = it_search_options.
  ENDMETHOD.

  METHOD zif_sat_object_search_query~get_option.
    rs_option = VALUE #( zif_sat_object_search_query~mt_search_options[ option = iv_option ] OPTIONAL ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~has_options.
    result = xsdbool( zif_sat_object_search_query~mt_search_options IS NOT INITIAL ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~has_search_terms.
    result = xsdbool( zif_sat_object_search_query~mt_search_term IS NOT INITIAL ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~set_option ##NEEDED.
  ENDMETHOD.

  METHOD zif_sat_object_search_query~set_max_rows ##NEEDED.
  ENDMETHOD.
ENDCLASS.


" Definition of unit test class
CLASS ltcl_abap_unit DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_os_subp_method_std.

    METHODS search_with_terms_only FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD search_with_terms_only.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) )
            ( target = zif_sat_c_os_meth_options=>c_search_fields-method_name_input_key
              values = VALUE #( ( sign = 'I' option = 'CP' low = 'PR*' ) ) ) )
        iv_type        = zif_sat_c_object_search=>c_search_type-method ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_equals( exp = 1
                                            act = lines( lt_result ) ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act  = lx_search_error
                                           quit = if_aunit_constants=>quit-test ).
  ENDMETHOD.
ENDCLASS.
