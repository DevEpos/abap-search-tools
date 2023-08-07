"! <p class="shorttext synchronized">Search provider for Class/Interface Methods</p>
CLASS zcl_sat_os_method_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_provider.

  PRIVATE SECTION.
    TYPES ty_providers TYPE TABLE OF REF TO zif_sat_object_search_provider WITH EMPTY KEY.

    DATA mo_query      TYPE REF TO zif_sat_object_search_query.
    DATA mt_result_all TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA: ms_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params.

    METHODS get_providers
      RETURNING
        VALUE(result) TYPE ty_providers.

    METHODS sort_result.
    METHODS limit_result.
ENDCLASS.


CLASS zcl_sat_os_method_provider IMPLEMENTATION.
  METHOD zif_sat_object_search_provider~search.
    mo_query = io_query.
    ms_search_engine_params = is_search_engine_params.

    " 1) Search standard methods
    " 2) Search redefined methods
    " 3) Search methods via changed admin data from includes
    LOOP AT get_providers( ) INTO DATA(lo_provider).
      lo_provider->search( EXPORTING io_query                = io_query
                                     is_search_engine_params = is_search_engine_params
                           IMPORTING et_result               = DATA(lt_result) ).

      mt_result_all = VALUE #( BASE mt_result_all ( LINES OF lt_result ) ).
    ENDLOOP.

    sort_result( ).
    limit_result( ).

    et_result = mt_result_all.
  ENDMETHOD.

  METHOD get_providers.
    DATA(lf_incl_admin_data) = VALUE #( ms_search_engine_params-custom_options[
                                            key = zif_sat_c_object_search=>c_custom_options-method-target_incl_for_admin_data ]-value OPTIONAL ).
    IF lf_incl_admin_data = abap_true.
      DATA(lo_meth_impl_provider) = NEW zcl_sat_os_subp_meth_impl( ).
      IF lo_meth_impl_provider->is_search_possible( mo_query ).
        result = VALUE #( ( lo_meth_impl_provider ) ).
        RETURN.
      ENDIF.
    ENDIF.

    result = VALUE #( ( NEW zcl_sat_os_subp_method_std( ) ) ).
  ENDMETHOD.

  METHOD sort_result.
    SORT mt_result_all BY object_name
                          method_level
                          method_exposure
                          method_name.
  ENDMETHOD.

  METHOD limit_result.
    DATA(lv_max_rows) = mo_query->mv_max_rows + 1.
    IF lines( mt_result_all ) > lv_max_rows.
      DELETE mt_result_all FROM lv_max_rows.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
