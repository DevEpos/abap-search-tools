"! <p class="shorttext synchronized">Search provider for Class/Interface Methods</p>
CLASS zcl_sat_os_method_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_provider.

  PRIVATE SECTION.
    TYPES ty_providers TYPE TABLE OF REF TO zif_sat_object_search_provider WITH EMPTY KEY.

    DATA mo_query TYPE REF TO zif_sat_object_search_query.
    DATA mt_result_all TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA ms_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params.

    METHODS get_providers
      RETURNING
        VALUE(result) TYPE ty_providers.

    METHODS sort_result.
    METHODS limit_result.

    METHODS get_status_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS check_redefined_search
      EXPORTING
        ef_redefined_active TYPE abap_bool
        ef_redefined_only   TYPE abap_bool.

    METHODS is_static_level_requested
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_abstract_requested
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_private_requested
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS zcl_sat_os_method_provider IMPLEMENTATION.
  METHOD zif_sat_object_search_provider~search.
    mo_query = io_query.
    ms_search_engine_params = is_search_engine_params.

    DATA(lt_search_providers) = get_providers( ).

    LOOP AT lt_search_providers INTO DATA(lo_provider).
      lo_provider->search( EXPORTING io_query                = io_query
                                     is_search_engine_params = is_search_engine_params
                           IMPORTING et_result               = DATA(lt_result) ).

      mt_result_all = VALUE #( BASE mt_result_all ( LINES OF lt_result ) ).
    ENDLOOP.

    IF lines( lt_search_providers ) > 1.
      sort_result( ).
      limit_result( ).
    ENDIF.

    et_result = mt_result_all.
  ENDMETHOD.

  METHOD get_providers.
    DATA(lf_incl_admin_data) = VALUE #( ms_search_engine_params-custom_options[
                                            key = zif_sat_c_object_search=>c_custom_options-method-target_incl_for_admin_data ]-value OPTIONAL ).
    IF lf_incl_admin_data = abap_true.
      DATA(lo_meth_impl_provider) = COND #( WHEN sy-dbsys = 'HDB'
                                            THEN NEW zcl_sat_os_subp_meth_impl( )
                                            ELSE NEW zcl_sat_os_subp_meth_impl_odb( ) ).
      IF lo_meth_impl_provider->is_search_possible( mo_query ).
        result = VALUE #( ( lo_meth_impl_provider ) ).
        RETURN.
      ENDIF.
    ENDIF.

    check_redefined_search( IMPORTING ef_redefined_active = DATA(lf_redefined_active)
                                      ef_redefined_only   = DATA(lf_redefined_only) ).

    IF lf_redefined_active = abap_true.
      result = VALUE #( ( NEW zcl_sat_os_subp_meth_redef( ) ) ).
    ENDIF.

    IF lf_redefined_only = abap_false.
      result = VALUE #( BASE result ( NEW zcl_sat_os_subp_method_std( ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD sort_result.
    SORT mt_result_all BY object_name
                          method_level DESCENDING
                          method_exposure DESCENDING
                          method_name.
    DELETE ADJACENT DUPLICATES FROM mt_result_all COMPARING object_name method_name.
  ENDMETHOD.

  METHOD limit_result.
    IF lines( mt_result_all ) > mo_query->mv_max_rows + 1.
      DELETE mt_result_all FROM mo_query->mv_max_rows + 2.
    ENDIF.
  ENDMETHOD.

  METHOD get_status_filter.
    result = VALUE #( mo_query->mt_search_options[
                          target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                          option = zif_sat_c_object_search=>c_method_search_option-status ]-value_range OPTIONAL ).
  ENDMETHOD.

  METHOD check_redefined_search.
    " check if 'redefined' methods should be search inclusively or exclusively
    DATA(lt_status_filter) = VALUE #( mo_query->mt_search_options[
                                          target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                          option = zif_sat_c_object_search=>c_method_search_option-status ]-value_range OPTIONAL ).

    IF lt_status_filter IS NOT INITIAL.
      DATA(lv_include_count) = 0.
      LOOP AT lt_status_filter INTO DATA(ls_status_filter).
        IF ls_status_filter-sign = 'I'.
          lv_include_count = lv_include_count + 1.
          IF ls_status_filter-low = zif_sat_c_object_search=>c_method_status_int-redefined.
            ef_redefined_active = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.

      ef_redefined_only = xsdbool( ef_redefined_active = abap_true AND lv_include_count = 1 ).
    ELSE.
      ef_redefined_active = abap_true.
    ENDIF.

    " check other filters as well to prevent unnecessary searches
    IF ef_redefined_active = abap_true.
      IF is_static_level_requested( ) OR is_abstract_requested( ) OR is_private_requested( ).
        CLEAR: ef_redefined_active,
               ef_redefined_only.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD is_static_level_requested.
    TYPES ty_level_filter TYPE RANGE OF seomtddecl.

    DATA(lt_level_filter) = VALUE #( mo_query->mt_search_options[
                                         target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                         option = zif_sat_c_object_search=>c_method_search_option-level ]-value_range OPTIONAL ).

    IF lt_level_filter IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_level_range) = CORRESPONDING ty_level_filter( lt_level_filter ).

    result = xsdbool(     seoo_mtddecltyp_class_method IN lt_level_range
                      AND seoo_mtddecltyp_method NOT   IN lt_level_range ).
  ENDMETHOD.

  METHOD is_abstract_requested.
    DATA(lt_flag_filter) = VALUE #( mo_query->mt_search_options[
                                        target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                        option = zif_sat_c_object_search=>c_method_search_option-flag ]-value_range OPTIONAL ).
    IF lt_flag_filter IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_flag_range) = CORRESPONDING zif_sat_ty_global=>ty_t_string_range( lt_flag_filter ).

    result = xsdbool(     zif_sat_c_object_search=>c_method_flags-abstract IN lt_flag_range
                      AND lines( lt_flag_filter ) = 1 ).
  ENDMETHOD.

  METHOD is_private_requested.
    DATA lt_visibility_range TYPE RANGE OF seoexpose.

    DATA(lt_visibility_filter) = VALUE #( mo_query->mt_search_options[
                                              target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                              option = zif_sat_c_object_search=>c_method_search_option-visibility ]-value_range OPTIONAL ).
    IF lt_visibility_filter IS INITIAL.
      RETURN.
    ENDIF.

    lt_visibility_range = CORRESPONDING #( lt_visibility_filter ).

    result = xsdbool(     seoc_exposure_private        IN lt_visibility_range
                      AND lines( lt_visibility_range )  = 1 ).
  ENDMETHOD.
ENDCLASS.
