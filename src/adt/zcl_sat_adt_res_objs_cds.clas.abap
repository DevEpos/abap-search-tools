"! <p class="shorttext synchronized" lang="en">Object Search Resource for CDS Search</p>
CLASS zcl_sat_adt_res_objs_cds DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_object_search
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_query_parameter
        REDEFINITION.
    METHODS post_process_result_entry
        REDEFINITION.
    METHODS post_process_result
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_objs_cds IMPLEMENTATION.

  METHOD get_query_parameter.
    DEFINE _get_param.
      get_search_parameter( EXPORTING io_request      = io_request
                                      iv_param_name   = &1
                                      iv_option_Name  = &2
                                      if_single_value = &3
                            CHANGING  ct_option       = et_options ).
    END-OF-DEFINITION.

    ev_type = zif_sat_c_object_browser_mode=>cds_view.

    ev_query = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-object_name
        io_request       = io_request
    ).
    es_search_engine_params = VALUE #(
      use_and_cond_for_options = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-use_and_for_filters
        iv_default_value = abap_false
        io_request       = io_request )
      with_api_state           = zcl_sat_adt_res_util=>get_request_param_value(
         iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-read_api_state
         iv_default_value = abap_false
         io_request       = io_request )
      get_all                  = zcl_sat_adt_res_util=>get_request_param_value(
         iv_param_name    = zif_sat_c_adt_utils=>c_search_query_parameter-get_all_results
         iv_default_value = abap_false
         io_request       = io_request )
    ).

    get_max_rows_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_user_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_release_state_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_description_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_type_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_package_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).
    get_field_name_param( EXPORTING io_request = io_request  CHANGING  ct_options = et_options ).

    _get_param:
        zif_sat_c_adt_utils=>c_search_query_parameter-select_from  zif_sat_c_object_browser=>c_search_option-by_select_from abap_false,
        zif_sat_c_adt_utils=>c_search_query_parameter-association  zif_sat_c_object_browser=>c_search_option-by_association abap_false,
        zif_sat_c_adt_utils=>c_search_query_parameter-annotation   zif_sat_c_object_browser=>c_search_option-by_anno        abap_false,
        zif_sat_c_adt_utils=>c_search_query_parameter-extended_by  zif_sat_c_object_browser=>c_search_option-by_extensions  abap_false,
        zif_sat_c_adt_utils=>c_search_query_parameter-param        zif_sat_c_object_browser=>c_search_option-by_param       abap_false,
        zif_sat_c_adt_utils=>c_search_query_parameter-params       zif_sat_c_object_browser=>c_search_option-by_params      abap_true.
  ENDMETHOD.

  METHOD post_process_result_entry.
    CHECK mt_ddls_source IS NOT INITIAL.

    set_ddl_positional_uri(
      EXPORTING is_result_entity = is_result_entity
      CHANGING  cs_result        = cs_result
    ).
  ENDMETHOD.

  METHOD post_process_result.
    DATA: lt_ddlname TYPE RANGE OF ddlname.

*.. Positional URI is no longer needed for where used starting with NW 7.54
    CHECK sy-saprl < 754.

*.. Read sources of all found DDLS search results to get row/column where the name of
*.... the entity is starting
    read_ddl_sources( ).
  ENDMETHOD.

ENDCLASS.
