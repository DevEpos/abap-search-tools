"! <p class="shorttext synchronized" lang="en">Object Search Resource for DB Table/View Search</p>
CLASS zcl_sat_adt_res_objs_dbtabview DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_object_search
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_query_parameter
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_objs_dbtabview IMPLEMENTATION.

  METHOD get_query_parameter.
    ev_type = zif_sat_c_object_browser_mode=>database_table_view.

    ev_query = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-object_name
        io_request       = io_request
    ).
    es_search_engine_params = VALUE #(
      use_and_cond_for_options = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-use_and_for_filters
        iv_default_value = abap_false
        io_request       = io_request )
      get_all                  = zcl_sat_adt_res_util=>get_request_param_value(
         iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-get_all_results
         iv_default_value = abap_false
         io_request       = io_request )
    ).

    get_max_rows_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_user_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
*    get_release_state_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_description_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_package_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_field_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
  ENDMETHOD.

ENDCLASS.
