"! <p class="shorttext synchronized">Where-Used-Analysis for Table/View/CDS in CDS Views</p>
CLASS zcl_sat_adt_res_cds_a_wusl DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

  PRIVATE SECTION.
    DATA mv_entity                  TYPE zsat_entity_id.
    DATA mf_only_local_assocs       TYPE abap_bool.
    DATA mv_source_origin           TYPE string.
    DATA mf_released_entitites_only TYPE abap_bool.

    METHODS create_response
      IMPORTING
        it_query_result TYPE zif_sat_ty_object_search=>ty_t_search_result
      RETURNING
        VALUE(result)   TYPE zif_sat_ty_adt_types=>ty_s_search_result.

    METHODS execute_object_search
      IMPORTING
        io_response TYPE REF TO if_adt_rest_response
      RAISING
        cx_adt_rest.

    METHODS get_additional_filters
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_search_option.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_a_wusl IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).
    execute_object_search( io_response = response ).
  ENDMETHOD.

  METHOD get_parameters.
    mv_entity = zcl_sat_adt_res_util=>get_request_param_value(
                    io_request    = io_request
                    iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-entity_name
                    if_mandatory  = abap_true ).
    mv_source_origin = zcl_sat_adt_res_util=>get_request_param_value(
                           io_request    = io_request
                           iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-source_origin
                           if_mandatory  = abap_true ).
    mf_released_entitites_only = zcl_sat_adt_res_util=>get_boolean_req_param(
        iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_released_entities
        io_request    = io_request ).
    mf_only_local_assocs = zcl_sat_adt_res_util=>get_boolean_req_param(
                               iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_local_assocs
                               io_request    = io_request ).
  ENDMETHOD.

  METHOD execute_object_search.
    TRY.
        DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                                                 iv_contract = 'zif_sat_search_engine' ) ).

        lo_search_engine->search_objects(
          EXPORTING iv_search_type          = zif_sat_c_object_search=>c_search_type-cds_view
                    it_options              = VALUE #(
                        ( target      = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                          option      = mv_source_origin
                          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = mv_entity ) ) )
                        ( LINES OF get_additional_filters( ) ) )
                    is_search_engine_params = VALUE #( with_api_state = abap_true
                                                       get_all        = abap_true )
          IMPORTING et_results              = DATA(lt_query_result) ).

      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_object_search
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_search_error ).
    ENDTRY.

    io_response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_search_result_ch( )
                                data            = create_response( lt_query_result ) ).
  ENDMETHOD.

  METHOD create_response.
    IF it_query_result IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_result_converter) = zcl_sat_adt_res_object_search=>create_result_converter(
                                    iv_search_type          = zif_sat_c_object_search=>c_search_type-cds_view
                                    if_no_package_hierarchy = abap_true
                                    it_query_result         = it_query_result ).
    result = lo_result_converter->convert( ).
  ENDMETHOD.

  METHOD get_additional_filters.
    IF mf_released_entitites_only = abap_true.
      result = VALUE #( ( option      = zif_sat_c_object_search=>c_general_search_params-release_state
                          target      = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'released' ) ) ) ).
    ENDIF.

    IF mf_only_local_assocs = abap_true.
      result = VALUE #( BASE result
                        ( option      = zif_sat_c_object_search=>c_cds_search_params-only_local_assocs
                          target      = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                          value_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'X' ) ) ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
