*"* use this source file for your ABAP unit test classes
* Mock class for Search Query
CLASS lcl_query DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_query.
    TYPES ty_search_term TYPE RANGE OF string.
    METHODS constructor
      IMPORTING
        it_search_term    TYPE ty_search_term OPTIONAL
        iv_type           TYPE zif_sat_ty_object_search=>ty_search_type OPTIONAL
        iv_max_rows       TYPE sy-tabix DEFAULT 50
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

  METHOD zif_sat_object_search_query~set_option ##needed.
  ENDMETHOD.

ENDCLASS.

*** Definition of unit test class
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mr_cut TYPE REF TO zcl_sat_os_classintf_provider.

    METHODS:
      search_with_terms_only FOR TESTING,
      search_description FOR TESTING,
      search_flags FOR TESTING,
      search_flags2 FOR TESTING,
      search_category FOR TESTING,
      search_abap_lang FOR TESTING,
      search_type FOR TESTING,
      search_with_friend FOR TESTING,
      search_with_super_type FOR TESTING,
      search_with_abap_lang FOR TESTING,
      search_with_intf_usage FOR TESTING,
      search_with_attribute FOR TESTING,
      search_with_method FOR TESTING,
      search_api FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD search_with_terms_only.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term    = VALUE #( ( sign = 'I' option = 'EQ' low = `CL_RIS_ADT_RES_APP` ) )
      iv_type           = zif_sat_c_object_search=>c_search_type-class_interface
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
        quit = if_aunit_constants=>quit-test
    ).
  ENDMETHOD.

  METHOD search_description.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_general_search_params-description
          value_range = VALUE #( ( low = to_upper( 'Special Listener for Self-Test' ) ) ) )
      )
      iv_type           = zif_sat_c_object_search=>c_search_type-class_interface
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
            quit = if_aunit_constants=>quit-test
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_abap_lang.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-abap_lang
          value_range = VALUE #( ( low = 'X' ) ) ) " Unicode
      )
      iv_type           = zif_sat_c_object_search=>c_search_type-class_interface
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_category.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term = VALUE #( ( sign = 'I' option = 'EQ' low = 'CX_REST_EXCEPTION' ) )
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-category
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = '40' ) ) ) " Exception
      )
      iv_type           = zif_sat_c_object_search=>c_search_type-class_interface
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals( act = lines( lt_result ) exp = 1 ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_flags.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term    = VALUE #( ( sign = 'I' option = 'EQ' low = 'CL_ADT_RES_NAMED_ITEMS' ) )
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-flag
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = zif_sat_c_object_search=>c_class_intf_flags-is_abstract )
                                 ( sign = 'I' option = 'EQ' low = zif_sat_c_object_search=>c_class_intf_flags-is_fixpoint )
                                 ( sign = 'E' option = 'EQ' low = zif_sat_c_object_search=>c_class_intf_flags-is_final ) )
        )
      )
      iv_type           = zif_sat_c_object_search=>c_search_type-class_interface
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
                    is_search_engine_params = VALUE #( use_and_cond_for_options = abap_true )
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_flags2.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term    = VALUE #( ( sign = 'I' option = 'EQ' low = 'CL_ADT_RES_NAMED_ITEMS' ) )
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-flag
          value_range = VALUE #( ( sign = 'E' option = 'EQ' low = zif_sat_c_object_search=>c_class_intf_flags-has_test ) )
        )
      )
      iv_type           = zif_sat_c_object_search=>c_search_type-class_interface
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
                    is_search_engine_params = VALUE #( use_and_cond_for_options = abap_true )
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_type.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_general_search_params-type
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = zif_sat_c_object_search=>c_class_types-interface ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).

        cl_abap_unit_assert=>assert_equals(
            act  = lt_result[ 1 ]-tadir_type
            exp  = zif_sat_c_object_search=>c_class_types-interface
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_with_friend.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-friend
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'IF_ALV_RM_GRID_FRIEND' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_with_super_type.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term    = VALUE #( ( sign = 'I' option = 'CP' low = 'CL_GUI_ALV*' ) )
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-super_type
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'CL_GUI_ALV_GRID_BASE' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals(
            act  = lines( lt_result )
            exp  = 1
        ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_with_abap_lang.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-abap_lang
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'X' )
                                 ( sign = 'I' option = 'EQ' low = '2' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_with_intf_usage.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-interface
          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'IF_AMDP_MARKER_HDB' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_not_initial( lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_with_attribute.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZCL_SAT_OBJECT_QUERY_PARSER' ) )
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-attribute
          value_range = VALUE #( ( sign  = 'I'  option  = 'EQ' low  = 'C_OPTION_SEPARATOR'
                                   sign2 = 'I' option2 = 'EQ' high = ':' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals( act = lines( lt_result ) exp = 1 ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_with_method.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_term = VALUE #( ( sign = 'I' option = 'EQ' low = 'CL_GUI_ALV_GRID' ) )
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_class_intf_search_option-method
          value_range = VALUE #( ( sign  = 'I'  option  = 'EQ' low  = 'GET_FRONTEND_LAYOUT' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_equals( act = lines( lt_result ) exp = 1 ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

  METHOD search_api.
    mr_cut = NEW #( ).

    DATA(lo_query) = NEW lcl_query(
      it_search_options = VALUE #(
        ( option      = zif_sat_c_object_search=>c_general_search_params-release_state
          value_range = VALUE #( ( sign  = 'I'  option  = 'EQ' low  = 'RELEASED' ) ) )
      )
      iv_type         = zif_sat_c_object_search=>c_search_type-class_interface
      iv_max_rows     = 1
    ).
    TRY.
        mr_cut->zif_sat_object_search_provider~search(
          EXPORTING io_query = lo_query
          IMPORTING et_result = DATA(lt_result)
        ).
        cl_abap_unit_assert=>assert_not_initial( act = lt_result ).
      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound(
        act  = lx_search_error
    ).
  ENDMETHOD.

ENDCLASS.
