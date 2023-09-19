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
    zif_sat_object_search_query~mv_type = zif_sat_c_object_search=>c_search_type-method.
    zif_sat_object_search_query~mv_max_rows = 1.
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


" Definition of unit test class for simple method filters
CLASS ltcl_method_filter_unit DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_os_subp_method_std.

    METHODS search_with_terms_only FOR TESTING.
    METHODS search_user            FOR TESTING.
    METHODS search_changedby       FOR TESTING.
    METHODS search_createdon       FOR TESTING.
    METHODS search_changedon       FOR TESTING.
    METHODS search_description     FOR TESTING.
    METHODS search_params          FOR TESTING.
    METHODS search_level           FOR TESTING.
    METHODS search_visibility      FOR TESTING.
    METHODS search_type            FOR TESTING.
    METHODS search_status          FOR TESTING.
    METHODS search_exception       FOR TESTING.
    METHODS search_flag            FOR TESTING.

ENDCLASS.


CLASS ltcl_method_filter_unit IMPLEMENTATION.
  METHOD search_with_terms_only.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) )
            ( target = zif_sat_c_object_search=>c_search_fields-method_name_input_key
              values = VALUE #( ( sign = 'I' option = 'CP' low = 'CONF*' ) ) ) )
        iv_type        = zif_sat_c_object_search=>c_search_type-method ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_equals( act = lines( lt_result )
                                            exp = 1 ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act  = lx_search_error
                                           quit = if_aunit_constants=>quit-test ).
  ENDMETHOD.

  METHOD search_user.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_general_search_params-changed_by
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign  = 'E'  option  = 'EQ' low  = 'SAP' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_changedby.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_general_search_params-changed_by
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign  = 'E'  option  = 'EQ' low  = 'SAP' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_createdon.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_general_search_params-created_on
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign  = 'E'  option  = 'EQ' low  = 'EEQ20990101' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_changedon.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_general_search_params-changed_by
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign  = 'E'  option  = 'EQ' low  = 'EEQ20000101' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_description.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #(
            ( option      = zif_sat_c_object_search=>c_general_search_params-description
              target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
              value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'Create filter for ATTR option' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_params.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_method_search_option-param
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign = 'I' option = 'CP' low = 'I*' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_level.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_method_search_option-level
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign = 'I' option = 'EQ' low = '0' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_visibility.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_method_search_option-level
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign = 'I' option = 'EQ' low = '0' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_type.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_method_search_option-level
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign = 'I' option = 'EQ' low = '0' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_status.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_method_search_option-status
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign = 'I' option = 'EQ' low = '1' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_exception.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
*                                    it_search_term    = value #(
*           ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
*                                    values            = value #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
                                    it_search_options = VALUE #(
                                        ( option      = zif_sat_c_object_search=>c_method_search_option-exception
                                          target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                          value_range = VALUE #( ( sign = 'I' option = 'CP' low = 'ZCX*' ) ) ) )
                                    iv_type           = zif_sat_c_object_search=>c_search_type-method
                                    iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_flag.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term    = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) )
        it_search_options = VALUE #( ( option      = zif_sat_c_object_search=>c_method_search_option-flag
                                       target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                       value_range = VALUE #( ( sign = 'E' option = 'EQ' low = 'ABSTRACT' ) ) ) )
        iv_type           = zif_sat_c_object_search=>c_search_type-method
        iv_max_rows       = 1 ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.
ENDCLASS.


" Definition of unit test class for all positive combinations of class and method filter
CLASS ltcl_method_class_filter_unit DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut            TYPE REF TO zcl_sat_os_subp_method_std.
    DATA mt_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option.
    DATA mt_search_terms   TYPE zif_sat_ty_object_search=>ty_t_search_term.

    METHODS setup.

    METHODS add_class_filter
      IMPORTING
        iv_filter TYPE string
        iv_value  TYPE string.

    METHODS add_method_filter
      IMPORTING
        iv_filter TYPE string
        iv_value  TYPE string.

    METHODS search_with_terms_only         FOR TESTING.
    METHODS search_cl_user_meth_user       FOR TESTING.
    METHODS search_cl_user_meth_createdon  FOR TESTING.
    METHODS search_cl_user_meth_changedby  FOR TESTING.
    METHODS search_cl_user_meth_changedon  FOR TESTING.
    METHODS search_cl_user_meth_desc       FOR TESTING.
    METHODS search_cl_user_meth_type       FOR TESTING.
    METHODS search_cl_user_meth_flag       FOR TESTING.
    METHODS search_cl_user_meth_param      FOR TESTING.
    METHODS search_cl_user_meth_exc        FOR TESTING.
    METHODS search_cl_user_meth_level      FOR TESTING.
    METHODS search_cl_user_meth_status     FOR TESTING.
    METHODS search_cl_user_meth_visib      FOR TESTING.
    METHODS search_cl_crton_meth_user      FOR TESTING.
    METHODS search_cl_crton_meth_createdon FOR TESTING.
    METHODS search_cl_crton_meth_changedby FOR TESTING.
    METHODS search_cl_crton_meth_changedon FOR TESTING.
    METHODS search_cl_crton_meth_desc      FOR TESTING.
    METHODS search_cl_crton_meth_type      FOR TESTING.
    METHODS search_cl_crton_meth_flag      FOR TESTING.
    METHODS search_cl_crton_meth_param     FOR TESTING.
    METHODS search_cl_crton_meth_exc       FOR TESTING.
    METHODS search_cl_crton_meth_level     FOR TESTING.
    METHODS search_cl_crton_meth_status    FOR TESTING.
    METHODS search_cl_crton_meth_visib     FOR TESTING.
    METHODS search_cl_chgby_meth_user      FOR TESTING.
    METHODS search_cl_chgby_meth_createdon FOR TESTING.
    METHODS search_cl_chgby_meth_changedby FOR TESTING.
    METHODS search_cl_chgby_meth_changedon FOR TESTING.
    METHODS search_cl_chgby_meth_desc      FOR TESTING.
    METHODS search_cl_chgby_meth_type      FOR TESTING.
    METHODS search_cl_chgby_meth_flag      FOR TESTING.
    METHODS search_cl_chgby_meth_param     FOR TESTING.
    METHODS search_cl_chgby_meth_exc       FOR TESTING.
    METHODS search_cl_chgby_meth_level     FOR TESTING.
    METHODS search_cl_chgby_meth_status    FOR TESTING.
    METHODS search_cl_chgby_meth_visib     FOR TESTING.
    METHODS search_cl_chgon_meth_user      FOR TESTING.
    METHODS search_cl_chgon_meth_createdon FOR TESTING.
    METHODS search_cl_chgon_meth_changedby FOR TESTING.
    METHODS search_cl_chgon_meth_changedon FOR TESTING.
    METHODS search_cl_chgon_meth_desc      FOR TESTING.
    METHODS search_cl_chgon_meth_type      FOR TESTING.
    METHODS search_cl_chgon_meth_flag      FOR TESTING.
    METHODS search_cl_chgon_meth_param     FOR TESTING.
    METHODS search_cl_chgon_meth_exc       FOR TESTING.
    METHODS search_cl_chgon_meth_level     FOR TESTING.
    METHODS search_cl_chgon_meth_status    FOR TESTING.
    METHODS search_cl_chgon_meth_visib     FOR TESTING.
    METHODS search_cl_pack_meth_user       FOR TESTING.
    METHODS search_cl_pack_meth_createdon  FOR TESTING.
    METHODS search_cl_pack_meth_changedby  FOR TESTING.
    METHODS search_cl_pack_meth_changedon  FOR TESTING.
    METHODS search_cl_pack_meth_desc       FOR TESTING.
    METHODS search_cl_pack_meth_type       FOR TESTING.
    METHODS search_cl_pack_meth_flag       FOR TESTING.
    METHODS search_cl_pack_meth_param      FOR TESTING.
    METHODS search_cl_pack_meth_exc        FOR TESTING.
    METHODS search_cl_pack_meth_level      FOR TESTING.
    METHODS search_cl_pack_meth_status     FOR TESTING.
    METHODS search_cl_pack_meth_visib      FOR TESTING.
    METHODS search_cl_comp_meth_user       FOR TESTING.
    METHODS search_cl_comp_meth_createdon  FOR TESTING.
    METHODS search_cl_comp_meth_changedby  FOR TESTING.
    METHODS search_cl_comp_meth_changedon  FOR TESTING.
    METHODS search_cl_comp_meth_desc       FOR TESTING.
    METHODS search_cl_comp_meth_type       FOR TESTING.
    METHODS search_cl_comp_meth_flag       FOR TESTING.
    METHODS search_cl_comp_meth_param      FOR TESTING.
    METHODS search_cl_comp_meth_exc        FOR TESTING.
    METHODS search_cl_comp_meth_level      FOR TESTING.
    METHODS search_cl_comp_meth_status     FOR TESTING.
    METHODS search_cl_comp_meth_visib      FOR TESTING.
    METHODS search_cl_appl_meth_user       FOR TESTING.
    METHODS search_cl_appl_meth_createdon  FOR TESTING.
    METHODS search_cl_appl_meth_changedby  FOR TESTING.
    METHODS search_cl_appl_meth_changedon  FOR TESTING.
    METHODS search_cl_appl_meth_desc       FOR TESTING.
    METHODS search_cl_appl_meth_type       FOR TESTING.
    METHODS search_cl_appl_meth_flag       FOR TESTING.
    METHODS search_cl_appl_meth_param      FOR TESTING.
    METHODS search_cl_appl_meth_exc        FOR TESTING.
    METHODS search_cl_appl_meth_level      FOR TESTING.
    METHODS search_cl_appl_meth_status     FOR TESTING.
    METHODS search_cl_appl_meth_visib      FOR TESTING.
    METHODS search_cl_desc_meth_user       FOR TESTING.
    METHODS search_cl_desc_meth_createdon  FOR TESTING.
    METHODS search_cl_desc_meth_changedby  FOR TESTING.
    METHODS search_cl_desc_meth_changedon  FOR TESTING.
    METHODS search_cl_desc_meth_desc       FOR TESTING.
    METHODS search_cl_desc_meth_type       FOR TESTING.
    METHODS search_cl_desc_meth_flag       FOR TESTING.
    METHODS search_cl_desc_meth_param      FOR TESTING.
    METHODS search_cl_desc_meth_exc        FOR TESTING.
    METHODS search_cl_desc_meth_level      FOR TESTING.
    METHODS search_cl_desc_meth_status     FOR TESTING.
    METHODS search_cl_desc_meth_visib      FOR TESTING.
    METHODS search_cl_type_meth_user       FOR TESTING.
    METHODS search_cl_type_meth_createdon  FOR TESTING.
    METHODS search_cl_type_meth_changedby  FOR TESTING.
    METHODS search_cl_type_meth_changedon  FOR TESTING.
    METHODS search_cl_type_meth_desc       FOR TESTING.
    METHODS search_cl_type_meth_type       FOR TESTING.
    METHODS search_cl_type_meth_flag       FOR TESTING.
    METHODS search_cl_type_meth_param      FOR TESTING.
    METHODS search_cl_type_meth_exc        FOR TESTING.
    METHODS search_cl_type_meth_level      FOR TESTING.
    METHODS search_cl_type_meth_status     FOR TESTING.
    METHODS search_cl_type_meth_visib      FOR TESTING.
    METHODS search_cl_flag_meth_user       FOR TESTING.
    METHODS search_cl_flag_meth_createdon  FOR TESTING.
    METHODS search_cl_flag_meth_changedby  FOR TESTING.
    METHODS search_cl_flag_meth_changedon  FOR TESTING.
    METHODS search_cl_flag_meth_desc       FOR TESTING.
    METHODS search_cl_flag_meth_type       FOR TESTING.
    METHODS search_cl_flag_meth_flag       FOR TESTING.
    METHODS search_cl_flag_meth_param      FOR TESTING.
    METHODS search_cl_flag_meth_exc        FOR TESTING.
    METHODS search_cl_flag_meth_level      FOR TESTING.
    METHODS search_cl_flag_meth_status     FOR TESTING.
    METHODS search_cl_flag_meth_visib      FOR TESTING.
    METHODS search_cl_cat_meth_user        FOR TESTING.
    METHODS search_cl_cat_meth_createdon   FOR TESTING.
    METHODS search_cl_cat_meth_changedby   FOR TESTING.
    METHODS search_cl_cat_meth_changedon   FOR TESTING.
    METHODS search_cl_cat_meth_desc        FOR TESTING.
    METHODS search_cl_cat_meth_type        FOR TESTING.
    METHODS search_cl_cat_meth_flag        FOR TESTING.
    METHODS search_cl_cat_meth_param       FOR TESTING.
    METHODS search_cl_cat_meth_exc         FOR TESTING.
    METHODS search_cl_cat_meth_level       FOR TESTING.
    METHODS search_cl_cat_meth_status      FOR TESTING.
    METHODS search_cl_cat_meth_visib       FOR TESTING.
    METHODS search_cl_intf_meth_user       FOR TESTING.
    METHODS search_cl_intf_meth_createdon  FOR TESTING.
    METHODS search_cl_intf_meth_changedby  FOR TESTING.
    METHODS search_cl_intf_meth_changedon  FOR TESTING.
    METHODS search_cl_intf_meth_desc       FOR TESTING.
    METHODS search_cl_intf_meth_type       FOR TESTING.
    METHODS search_cl_intf_meth_flag       FOR TESTING.
    METHODS search_cl_intf_meth_param      FOR TESTING.
    METHODS search_cl_intf_meth_exc        FOR TESTING.
    METHODS search_cl_intf_meth_level      FOR TESTING.
    METHODS search_cl_intf_meth_status     FOR TESTING.
    METHODS search_cl_intf_meth_visib      FOR TESTING.
    METHODS search_cl_frind_meth_user      FOR TESTING.
    METHODS search_cl_frind_meth_createdon FOR TESTING.
    METHODS search_cl_frind_meth_changedby FOR TESTING.
    METHODS search_cl_frind_meth_changedon FOR TESTING.
    METHODS search_cl_frind_meth_desc      FOR TESTING.
    METHODS search_cl_frind_meth_type      FOR TESTING.
    METHODS search_cl_frind_meth_flag      FOR TESTING.
    METHODS search_cl_frind_meth_param     FOR TESTING.
    METHODS search_cl_frind_meth_exc       FOR TESTING.
    METHODS search_cl_frind_meth_level     FOR TESTING.
    METHODS search_cl_frind_meth_status    FOR TESTING.
    METHODS search_cl_frind_meth_visib     FOR TESTING.
    METHODS search_cl_super_meth_user      FOR TESTING.
    METHODS search_cl_super_meth_createdon FOR TESTING.
    METHODS search_cl_super_meth_changedby FOR TESTING.
    METHODS search_cl_super_meth_changedon FOR TESTING.
    METHODS search_cl_super_meth_desc      FOR TESTING.
    METHODS search_cl_super_meth_type      FOR TESTING.
    METHODS search_cl_super_meth_flag      FOR TESTING.
    METHODS search_cl_super_meth_param     FOR TESTING.
    METHODS search_cl_super_meth_exc       FOR TESTING.
    METHODS search_cl_super_meth_level     FOR TESTING.
    METHODS search_cl_super_meth_status    FOR TESTING.
    METHODS search_cl_super_meth_visib     FOR TESTING.

ENDCLASS.


CLASS ltcl_method_class_filter_unit IMPLEMENTATION.
  METHOD setup.
    mt_search_terms = VALUE #(
        ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
          values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) ).
  ENDMETHOD.

  METHOD add_class_filter.
    mt_search_options = VALUE #( BASE mt_search_options
                                 ( option      = iv_filter
                                   target      = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                   value_range = VALUE #( ( sign  = 'I'  option  = 'EQ' low  = iv_value ) ) ) ).
  ENDMETHOD.

  METHOD add_method_filter.
    mt_search_options = VALUE #( BASE mt_search_options
                                 ( option      = iv_filter
                                   target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                   value_range = VALUE #( ( sign  = 'I'  option  = 'EQ' low  = iv_value ) ) ) ).
  ENDMETHOD.

  METHOD search_with_terms_only.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) )
            ( target = zif_sat_c_object_search=>c_search_fields-method_name_input_key
              values = VALUE #( ( sign = 'I' option = 'CP' low = 'CONF*' ) ) ) )
        iv_type        = zif_sat_c_object_search=>c_search_type-method ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_equals( act = lines( lt_result )
                                            exp = 1 ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act  = lx_search_error
                                           quit = if_aunit_constants=>quit-test ).
  ENDMETHOD.

  METHOD search_cl_user_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.
ENDCLASS.


" Definition of unit test class for all positive combinations of class and method filter
CLASS ltcl_neg_meth_class_fltr_unit DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut            TYPE REF TO zcl_sat_os_subp_method_std.
    DATA mt_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option.
    DATA mt_search_terms   TYPE zif_sat_ty_object_search=>ty_t_search_term.

    METHODS setup.

    METHODS add_class_filter
      IMPORTING
        iv_filter TYPE string
        iv_value  TYPE string.

    METHODS add_method_filter
      IMPORTING
        iv_filter TYPE string
        iv_value  TYPE string.

    METHODS search_with_terms_only         FOR TESTING.
    METHODS search_cl_user_meth_user       FOR TESTING.
    METHODS search_cl_user_meth_createdon  FOR TESTING.
    METHODS search_cl_user_meth_changedby  FOR TESTING.
    METHODS search_cl_user_meth_changedon  FOR TESTING.
    METHODS search_cl_user_meth_desc       FOR TESTING.
    METHODS search_cl_user_meth_type       FOR TESTING.
    METHODS search_cl_user_meth_flag       FOR TESTING.
    METHODS search_cl_user_meth_param      FOR TESTING.
    METHODS search_cl_user_meth_exc        FOR TESTING.
    METHODS search_cl_user_meth_level      FOR TESTING.
    METHODS search_cl_user_meth_status     FOR TESTING.
    METHODS search_cl_user_meth_visib      FOR TESTING.
    METHODS search_cl_crton_meth_user      FOR TESTING.
    METHODS search_cl_crton_meth_createdon FOR TESTING.
    METHODS search_cl_crton_meth_changedby FOR TESTING.
    METHODS search_cl_crton_meth_changedon FOR TESTING.
    METHODS search_cl_crton_meth_desc      FOR TESTING.
    METHODS search_cl_crton_meth_type      FOR TESTING.
    METHODS search_cl_crton_meth_flag      FOR TESTING.
    METHODS search_cl_crton_meth_param     FOR TESTING.
    METHODS search_cl_crton_meth_exc       FOR TESTING.
    METHODS search_cl_crton_meth_level     FOR TESTING.
    METHODS search_cl_crton_meth_status    FOR TESTING.
    METHODS search_cl_crton_meth_visib     FOR TESTING.
    METHODS search_cl_chgby_meth_user      FOR TESTING.
    METHODS search_cl_chgby_meth_createdon FOR TESTING.
    METHODS search_cl_chgby_meth_changedby FOR TESTING.
    METHODS search_cl_chgby_meth_changedon FOR TESTING.
    METHODS search_cl_chgby_meth_desc      FOR TESTING.
    METHODS search_cl_chgby_meth_type      FOR TESTING.
    METHODS search_cl_chgby_meth_flag      FOR TESTING.
    METHODS search_cl_chgby_meth_param     FOR TESTING.
    METHODS search_cl_chgby_meth_exc       FOR TESTING.
    METHODS search_cl_chgby_meth_level     FOR TESTING.
    METHODS search_cl_chgby_meth_status    FOR TESTING.
    METHODS search_cl_chgby_meth_visib     FOR TESTING.
    METHODS search_cl_chgon_meth_user      FOR TESTING.
    METHODS search_cl_chgon_meth_createdon FOR TESTING.
    METHODS search_cl_chgon_meth_changedby FOR TESTING.
    METHODS search_cl_chgon_meth_changedon FOR TESTING.
    METHODS search_cl_chgon_meth_desc      FOR TESTING.
    METHODS search_cl_chgon_meth_type      FOR TESTING.
    METHODS search_cl_chgon_meth_flag      FOR TESTING.
    METHODS search_cl_chgon_meth_param     FOR TESTING.
    METHODS search_cl_chgon_meth_exc       FOR TESTING.
    METHODS search_cl_chgon_meth_level     FOR TESTING.
    METHODS search_cl_chgon_meth_status    FOR TESTING.
    METHODS search_cl_chgon_meth_visib     FOR TESTING.
    METHODS search_cl_pack_meth_user       FOR TESTING.
    METHODS search_cl_pack_meth_createdon  FOR TESTING.
    METHODS search_cl_pack_meth_changedby  FOR TESTING.
    METHODS search_cl_pack_meth_changedon  FOR TESTING.
    METHODS search_cl_pack_meth_desc       FOR TESTING.
    METHODS search_cl_pack_meth_type       FOR TESTING.
    METHODS search_cl_pack_meth_flag       FOR TESTING.
    METHODS search_cl_pack_meth_param      FOR TESTING.
    METHODS search_cl_pack_meth_exc        FOR TESTING.
    METHODS search_cl_pack_meth_level      FOR TESTING.
    METHODS search_cl_pack_meth_status     FOR TESTING.
    METHODS search_cl_pack_meth_visib      FOR TESTING.
    METHODS search_cl_comp_meth_user       FOR TESTING.
    METHODS search_cl_comp_meth_createdon  FOR TESTING.
    METHODS search_cl_comp_meth_changedby  FOR TESTING.
    METHODS search_cl_comp_meth_changedon  FOR TESTING.
    METHODS search_cl_comp_meth_desc       FOR TESTING.
    METHODS search_cl_comp_meth_type       FOR TESTING.
    METHODS search_cl_comp_meth_flag       FOR TESTING.
    METHODS search_cl_comp_meth_param      FOR TESTING.
    METHODS search_cl_comp_meth_exc        FOR TESTING.
    METHODS search_cl_comp_meth_level      FOR TESTING.
    METHODS search_cl_comp_meth_status     FOR TESTING.
    METHODS search_cl_comp_meth_visib      FOR TESTING.
    METHODS search_cl_appl_meth_user       FOR TESTING.
    METHODS search_cl_appl_meth_createdon  FOR TESTING.
    METHODS search_cl_appl_meth_changedby  FOR TESTING.
    METHODS search_cl_appl_meth_changedon  FOR TESTING.
    METHODS search_cl_appl_meth_desc       FOR TESTING.
    METHODS search_cl_appl_meth_type       FOR TESTING.
    METHODS search_cl_appl_meth_flag       FOR TESTING.
    METHODS search_cl_appl_meth_param      FOR TESTING.
    METHODS search_cl_appl_meth_exc        FOR TESTING.
    METHODS search_cl_appl_meth_level      FOR TESTING.
    METHODS search_cl_appl_meth_status     FOR TESTING.
    METHODS search_cl_appl_meth_visib      FOR TESTING.
    METHODS search_cl_desc_meth_user       FOR TESTING.
    METHODS search_cl_desc_meth_createdon  FOR TESTING.
    METHODS search_cl_desc_meth_changedby  FOR TESTING.
    METHODS search_cl_desc_meth_changedon  FOR TESTING.
    METHODS search_cl_desc_meth_desc       FOR TESTING.
    METHODS search_cl_desc_meth_type       FOR TESTING.
    METHODS search_cl_desc_meth_flag       FOR TESTING.
    METHODS search_cl_desc_meth_param      FOR TESTING.
    METHODS search_cl_desc_meth_exc        FOR TESTING.
    METHODS search_cl_desc_meth_level      FOR TESTING.
    METHODS search_cl_desc_meth_status     FOR TESTING.
    METHODS search_cl_desc_meth_visib      FOR TESTING.
    METHODS search_cl_type_meth_user       FOR TESTING.
    METHODS search_cl_type_meth_createdon  FOR TESTING.
    METHODS search_cl_type_meth_changedby  FOR TESTING.
    METHODS search_cl_type_meth_changedon  FOR TESTING.
    METHODS search_cl_type_meth_desc       FOR TESTING.
    METHODS search_cl_type_meth_type       FOR TESTING.
    METHODS search_cl_type_meth_flag       FOR TESTING.
    METHODS search_cl_type_meth_param      FOR TESTING.
    METHODS search_cl_type_meth_exc        FOR TESTING.
    METHODS search_cl_type_meth_level      FOR TESTING.
    METHODS search_cl_type_meth_status     FOR TESTING.
    METHODS search_cl_type_meth_visib      FOR TESTING.
    METHODS search_cl_flag_meth_user       FOR TESTING.
    METHODS search_cl_flag_meth_createdon  FOR TESTING.
    METHODS search_cl_flag_meth_changedby  FOR TESTING.
    METHODS search_cl_flag_meth_changedon  FOR TESTING.
    METHODS search_cl_flag_meth_desc       FOR TESTING.
    METHODS search_cl_flag_meth_type       FOR TESTING.
    METHODS search_cl_flag_meth_flag       FOR TESTING.
    METHODS search_cl_flag_meth_param      FOR TESTING.
    METHODS search_cl_flag_meth_exc        FOR TESTING.
    METHODS search_cl_flag_meth_level      FOR TESTING.
    METHODS search_cl_flag_meth_status     FOR TESTING.
    METHODS search_cl_flag_meth_visib      FOR TESTING.
    METHODS search_cl_cat_meth_user        FOR TESTING.
    METHODS search_cl_cat_meth_createdon   FOR TESTING.
    METHODS search_cl_cat_meth_changedby   FOR TESTING.
    METHODS search_cl_cat_meth_changedon   FOR TESTING.
    METHODS search_cl_cat_meth_desc        FOR TESTING.
    METHODS search_cl_cat_meth_type        FOR TESTING.
    METHODS search_cl_cat_meth_flag        FOR TESTING.
    METHODS search_cl_cat_meth_param       FOR TESTING.
    METHODS search_cl_cat_meth_exc         FOR TESTING.
    METHODS search_cl_cat_meth_level       FOR TESTING.
    METHODS search_cl_cat_meth_status      FOR TESTING.
    METHODS search_cl_cat_meth_visib       FOR TESTING.
    METHODS search_cl_intf_meth_user       FOR TESTING.
    METHODS search_cl_intf_meth_createdon  FOR TESTING.
    METHODS search_cl_intf_meth_changedby  FOR TESTING.
    METHODS search_cl_intf_meth_changedon  FOR TESTING.
    METHODS search_cl_intf_meth_desc       FOR TESTING.
    METHODS search_cl_intf_meth_type       FOR TESTING.
    METHODS search_cl_intf_meth_flag       FOR TESTING.
    METHODS search_cl_intf_meth_param      FOR TESTING.
    METHODS search_cl_intf_meth_exc        FOR TESTING.
    METHODS search_cl_intf_meth_level      FOR TESTING.
    METHODS search_cl_intf_meth_status     FOR TESTING.
    METHODS search_cl_intf_meth_visib      FOR TESTING.
    METHODS search_cl_frind_meth_user      FOR TESTING.
    METHODS search_cl_frind_meth_createdon FOR TESTING.
    METHODS search_cl_frind_meth_changedby FOR TESTING.
    METHODS search_cl_frind_meth_changedon FOR TESTING.
    METHODS search_cl_frind_meth_desc      FOR TESTING.
    METHODS search_cl_frind_meth_type      FOR TESTING.
    METHODS search_cl_frind_meth_flag      FOR TESTING.
    METHODS search_cl_frind_meth_param     FOR TESTING.
    METHODS search_cl_frind_meth_exc       FOR TESTING.
    METHODS search_cl_frind_meth_level     FOR TESTING.
    METHODS search_cl_frind_meth_status    FOR TESTING.
    METHODS search_cl_frind_meth_visib     FOR TESTING.
    METHODS search_cl_super_meth_user      FOR TESTING.
    METHODS search_cl_super_meth_createdon FOR TESTING.
    METHODS search_cl_super_meth_changedby FOR TESTING.
    METHODS search_cl_super_meth_changedon FOR TESTING.
    METHODS search_cl_super_meth_desc      FOR TESTING.
    METHODS search_cl_super_meth_type      FOR TESTING.
    METHODS search_cl_super_meth_flag      FOR TESTING.
    METHODS search_cl_super_meth_param     FOR TESTING.
    METHODS search_cl_super_meth_exc       FOR TESTING.
    METHODS search_cl_super_meth_level     FOR TESTING.
    METHODS search_cl_super_meth_status    FOR TESTING.
    METHODS search_cl_super_meth_visib     FOR TESTING.

ENDCLASS.


CLASS ltcl_neg_meth_class_fltr_unit IMPLEMENTATION.
  METHOD setup.
    mt_search_terms = VALUE #(
        ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
          values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) ) ).
  ENDMETHOD.

  METHOD add_class_filter.
    mt_search_options = VALUE #( BASE mt_search_options
                                 ( option      = iv_filter
                                   target      = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                   value_range = VALUE #( ( sign  = 'I'  option  = 'EQ' low  = iv_value ) ) ) ).
  ENDMETHOD.

  METHOD add_method_filter.
    mt_search_options = VALUE #( BASE mt_search_options
                                 ( option      = iv_filter
                                   target      = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                   value_range = VALUE #( ( sign  = 'E'  option  = 'EQ' low  = iv_value ) ) ) ).
  ENDMETHOD.

  METHOD search_with_terms_only.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
        it_search_term = VALUE #(
            ( target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
              values = VALUE #( ( sign = 'I' option = 'EQ' low = `ZCL_SAT_OS_CLASSINTF_PROVIDER` ) ) )
            ( target = zif_sat_c_object_search=>c_search_fields-method_name_input_key
              values = VALUE #( ( sign = 'I' option = 'CP' low = 'CONF*' ) ) ) )
        iv_type        = zif_sat_c_object_search=>c_search_type-method ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_equals( act = lines( lt_result )
                                            exp = 1 ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act  = lx_search_error
                                           quit = if_aunit_constants=>quit-test ).
  ENDMETHOD.

  METHOD search_cl_user_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_user_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_crton_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgby_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_chgon_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_pack_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-package iv_value = 'BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_comp_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-software_component iv_value = 'SAP_BASIS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_appl_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-application_component iv_value = 'BC' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_desc_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                      iv_value  = 'Search provider for ABAP OO Classes/interfaces' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_type_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = 'CLAS' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_flag_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-flag iv_value = 'ABSTRACT' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_cat_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-category iv_value = '00' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_intf_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-interface iv_value = 'ZIF_SAT_C_JOIN_COND_TYPE' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_frind_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-friend iv_value = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_user.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-user iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_createdon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-created_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_changedby.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_by iv_value = 'SAP' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_changedon.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-changed_on iv_value = 'IEQ20000101' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_desc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-description
                       iv_value  = 'Create filter for ATTR option' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_type.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_general_search_params-type iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_flag.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-flag iv_value = 'ABSTRACT' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_param.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-param iv_value = 'IV_SUBQUERY' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_exc.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-exception iv_value = 'ZCX_SAT_OBJECT_SEARCH' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_level.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-level iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_status.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-status iv_value = '1' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.

  METHOD search_cl_super_meth_visib.
    mr_cut = NEW #( ).

    CLEAR mt_search_options.
    add_class_filter( iv_filter = zif_sat_c_object_search=>c_class_intf_search_option-super_type
                      iv_value  = 'ZCL_SAT_BASE_SEARCH_PROVIDER' ).
    add_method_filter( iv_filter = zif_sat_c_object_search=>c_method_search_option-visibility iv_value = '0' ).

    DATA(lo_query) = NEW lcl_query( it_search_term = mt_search_terms it_search_options = mt_search_options ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search( EXPORTING io_query  = lo_query
                                                       IMPORTING et_result = DATA(lt_result) ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lx_search_error ).
  ENDMETHOD.
ENDCLASS.
