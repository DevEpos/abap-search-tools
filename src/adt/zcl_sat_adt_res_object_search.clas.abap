CLASS zcl_sat_adt_res_object_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

    METHODS constructor
      IMPORTING
        iv_search_type TYPE zif_sat_ty_object_search=>ty_search_type.

    "! <p class="shorttext synchronized">Creates result converter for object search result</p>
    CLASS-METHODS create_result_converter
      IMPORTING
        iv_search_type          TYPE zif_sat_ty_object_search=>ty_search_type
        if_no_package_hierarchy TYPE abap_bool OPTIONAL
        it_query_result         TYPE zif_sat_ty_object_search=>ty_t_search_result
      RETURNING
        VALUE(result)           TYPE REF TO zif_sat_adt_objs_res_converter.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_package,
        package      TYPE devclass,
        createdby    TYPE username,
        superpackage TYPE devclass,
        description  TYPE ddtext,
      END OF ty_s_package,
      BEGIN OF ty_s_ddls_source,
        ddlname TYPE ddlname,
        source  TYPE string,
      END OF ty_s_ddls_source,
      ty_lt_ddlname TYPE RANGE OF ddlname.

    DATA mt_query_result           TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA mf_with_package_hierarchy TYPE abap_bool.
    DATA ms_query_input            TYPE zif_sat_ty_adt_types=>ty_s_query_input.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Create response</p>
    METHODS create_response
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_s_search_result.
ENDCLASS.


CLASS zcl_sat_adt_res_object_search IMPLEMENTATION.
  METHOD create_result_converter.
    DATA(lo_result_converter) = lcl_result_converter_factory=>create_result_converter(
                                    iv_search_type  = iv_search_type
                                    it_query_result = it_query_result ).
    lo_result_converter->set_no_package_hiearchy( if_no_package_hierarchy ).
    result = CAST #( lo_result_converter ).
  ENDMETHOD.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = zcl_sat_adt_ch_factory=>create_query_input_ch( )
                            IMPORTING data            = ms_query_input ).

    TRY.

        DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                                                 iv_contract = 'zif_sat_search_engine' ) ).
        lo_search_engine->search_objects(
          EXPORTING it_search_terms         = get_search_terms( )
                    iv_max_rows             = ms_query_input-max_rows
                    iv_search_type          = ms_query_input-type
                    it_options              = get_options( )
                    is_search_engine_params = VALUE #(
                        use_and_cond_for_options = ms_query_input-combine_filters_with_and
                        with_api_state           = ms_query_input-with_api_state
                        get_all                  = ms_query_input-no_row_limit )
          IMPORTING et_results              = mt_query_result ).

      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_object_search
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_search_error ).
    ENDTRY.

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_search_result_ch( )
                             data            = create_response( ) ).
  ENDMETHOD.

  METHOD get_query_parameter.
    ev_query = zcl_sat_adt_res_util=>get_request_param_value(
                   iv_param_name = zif_sat_c_object_search=>c_general_search_params-object_name
                   io_request    = io_request ).
    es_search_engine_params = VALUE #(
        use_and_cond_for_options = zcl_sat_adt_res_util=>get_request_param_value(
            iv_param_name    = zif_sat_c_object_search=>c_general_search_params-use_and_for_filters
            iv_default_value = abap_false
            io_request       = io_request )
        with_api_state           = zcl_sat_adt_res_util=>get_request_param_value(
            iv_param_name    = zif_sat_c_object_search=>c_general_search_params-read_api_state
            iv_default_value = abap_false
            io_request       = io_request )
        get_all                  = zcl_sat_adt_res_util=>get_request_param_value(
            iv_param_name    = zif_sat_c_object_search=>c_general_search_params-get_all_results
            iv_default_value = abap_false
            io_request       = io_request ) ).

    LOOP AT mo_search_config->get_options( ) ASSIGNING FIELD-SYMBOL(<ls_option>).
      get_search_parameter( EXPORTING io_request      = io_request
                                      iv_param_name   = <ls_option>-name
                                      if_single_value = <ls_option>-single
                            CHANGING  ct_option       = et_options ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_response.
    IF mt_query_result IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_result_converter) = lcl_result_converter_factory=>create_result_converter(
                                    iv_search_type  = mv_search_type
                                    it_query_result = mt_query_result ).
    result = lo_result_converter->zif_sat_adt_objs_res_converter~convert( ).
  ENDMETHOD.

  METHOD get_request_param_value.
    TRY.
        io_request->get_uri_query_parameter( EXPORTING name  = iv_param_name
                                             IMPORTING value = rv_value ).
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.

  METHOD get_request_param_values.
    TRY.
        io_request->get_uri_query_parameter_values( EXPORTING name   = iv_param_name
                                                    IMPORTING values = rt_values ).
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.

  METHOD get_search_parameter.
    DATA(lv_option_name) = COND #( WHEN iv_option_name IS NOT INITIAL THEN iv_option_name ELSE iv_param_name ).
    IF if_single_value = abap_true.
      DATA(lv_param_value) = get_request_param_value( iv_param_name = iv_param_name
                                                      io_request    = io_request ).
      IF lv_param_value IS NOT INITIAL.
        ct_option = VALUE #( BASE ct_option
                             ( option      = lv_option_name
                               value_range = VALUE #( ( low = lv_param_value ) ) ) ).
      ENDIF.
    ELSE.
      DATA(lt_param_values) = get_request_param_values( iv_param_name = iv_param_name
                                                        io_request    = io_request ).
      IF lt_param_values IS NOT INITIAL.
        ct_option = VALUE #( BASE ct_option
                             ( option      = lv_option_name
                               value_range = VALUE #( FOR value IN lt_param_values ( low = value ) ) ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
