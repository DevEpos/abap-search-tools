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
ENDCLASS.


CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_os_cds_provider.

    METHODS test_negation_query   FOR TESTING.
    METHODS test_anno_exclusion   FOR TESTING.
    METHODS test_only_local_annos FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD test_negation_query.
    mr_cut = NEW #( ).

    TRY.
        DATA(lo_query) = NEW lcl_query( iv_type           = zif_sat_c_object_search=>c_search_type-cds_view
                                        it_search_options = VALUE #(
                                            ( option      = zif_sat_c_object_search=>c_cds_search_params-annotation
                                              value_range = VALUE #(
                                                  sign   = 'E'
                                                  option = 'CP'
                                                  ( low = 'objectmodel.*usagetype.*sizecategory' )
                                                  ( low = 'vmd*.private' option2 = 'EQ' high = 'true' ) ) ) ) ).
        mr_cut->zif_sat_object_search_provider~search( io_query = lo_query ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD test_only_local_annos.
    mr_cut = NEW #( ).

    TRY.
        DATA(lo_query) = NEW lcl_query(
                                 iv_type           = zif_sat_c_object_search=>c_search_type-cds_view
                                 it_search_options = VALUE #(
                                     ( option      = zif_sat_c_object_search=>c_cds_search_params-only_local_assocs
                                       value_range = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ) )
                                     ( option      = zif_sat_c_object_search=>c_cds_search_params-association
                                       value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'I_PRODUCT' ) ) ) ) ).
        mr_cut->zif_sat_object_search_provider~search( io_query = lo_query ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD test_anno_exclusion.
    mr_cut = NEW #( ).

    TRY.
        DATA(lo_query) = NEW lcl_query( iv_type           = zif_sat_c_object_search=>c_search_type-cds_view
                                        iv_query          = 'C_'
                                        it_search_options = VALUE #(
                                            ( option      = zif_sat_c_object_search=>c_cds_search_params-annotation
                                              value_range = VALUE #(
                                                  sign   = 'E'
                                                  option = 'CP'
                                                  ( low = 'objectmodel.*usagetype' )
                                                  ( low = 'vmd*.private' option2 = 'EQ' high = 'true' ) ) ) ) ).
        mr_cut->zif_sat_object_search_provider~search( io_query = lo_query ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.
ENDCLASS.
