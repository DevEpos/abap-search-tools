*"* use this source file for your ABAP unit test classes
CLASS lcl_query DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_query.

    TYPES ty_search_term TYPE RANGE OF string.

    METHODS constructor
      IMPORTING
        it_search_term    TYPE zif_sat_ty_object_search=>ty_t_search_term   OPTIONAL
        iv_type           TYPE zif_sat_ty_object_search=>ty_search_type     OPTIONAL
        iv_query          TYPE string                                       OPTIONAL
        iv_max_rows       TYPE sy-tabix                                     OPTIONAL
        it_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL.
ENDCLASS.


CLASS lcl_query IMPLEMENTATION.
  METHOD constructor.
    zif_sat_object_search_query~mt_search_term = it_search_term.
    zif_sat_object_search_query~mv_type = iv_type.
    zif_sat_object_search_query~mv_query = iv_query.
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


CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_sat_os_subp_meth_impl_odb.

    METHODS setup.
    METHODS test_method FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_method.
    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #( ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                       values = VALUE #( ( sign = 'I' option = 'CP' low = 'ZCL_*' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1
        it_search_options = VALUE #(
            ( target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
              option      = zif_sat_c_object_search=>c_general_search_params-changed_on
              value_range = VALUE #( ( sign = 'I' option = 'EQ' low = |IEQ{ sy-datum DATE = RAW }| ) ) ) ) ).

    TRY.
        cut->zif_sat_object_search_provider~search(
          EXPORTING io_query                = lo_query
                    is_search_engine_params = VALUE #(
                        custom_options = VALUE #(
                            ( key   = zif_sat_c_object_search=>c_custom_options-method-target_incl_for_admin_data
                              value = abap_true ) ) )
          IMPORTING et_result               = DATA(lt_result) ).
      CATCH zcx_sat_object_search INTO DATA(lx_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_error  ).
  ENDMETHOD.
ENDCLASS.
