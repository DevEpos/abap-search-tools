"! <p class="shorttext synchronized" lang="en">Resource for Class search</p>
CLASS zcl_sat_adt_res_objs_classintf DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_object_search
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS get_query_parameter
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_objs_classintf IMPLEMENTATION.


  METHOD get_query_parameter.
    DEFINE _get_param.
      get_search_parameter( EXPORTING io_request      = io_request
                                      iv_param_name   = &1
                            CHANGING  ct_option       = et_options ).
    END-OF-DEFINITION.

    ev_type = zif_sat_c_object_search=>c_search_type-class_interface.

    ev_query = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_general_search_params-object_name
        io_request       = io_request
    ).
    es_search_engine_params = VALUE #(
      use_and_cond_for_options = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_general_search_params-use_and_for_filters
        iv_default_value = abap_false
        io_request       = io_request )
      get_all                  = zcl_sat_adt_res_util=>get_request_param_value(
         iv_param_name    = zif_sat_c_adt_utils=>c_general_search_params-get_all_results
         iv_default_value = abap_false
         io_request       = io_request )
    ).

    get_max_rows_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_user_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
*    get_release_state_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_description_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_package_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).

    _get_param: zif_sat_c_object_search=>c_class_intf_search_option-by_method,
                zif_sat_c_object_search=>c_class_intf_search_option-by_attribute,
                zif_sat_c_object_search=>c_class_intf_search_option-by_super_type,
                zif_sat_c_object_search=>c_class_intf_search_option-by_sub_type,
                zif_sat_c_object_search=>c_class_intf_search_option-by_friend.
  ENDMETHOD.
ENDCLASS.
