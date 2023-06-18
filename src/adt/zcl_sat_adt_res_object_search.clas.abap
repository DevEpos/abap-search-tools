CLASS zcl_sat_adt_res_object_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

    METHODS constructor
      IMPORTING
        iv_search_type TYPE zif_sat_ty_object_search=>ty_search_type.

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
    DATA mv_search_type            TYPE zif_sat_ty_object_search=>ty_search_type.
    DATA mt_super_package_all      TYPE SORTED TABLE OF ty_s_package WITH UNIQUE KEY package.
    DATA mt_ddls_source            TYPE HASHED TABLE OF ty_s_ddls_source WITH UNIQUE KEY ddlname.
    DATA mo_search_config          TYPE REF TO zif_sat_object_search_config.

    "! <p class="shorttext synchronized">Retrieve query parameters from request</p>
    METHODS get_query_parameter
      IMPORTING
        io_request              TYPE REF TO if_adt_rest_request
      EXPORTING
        ev_query                TYPE string
        es_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params
        et_options              TYPE zif_sat_ty_object_search=>ty_t_search_option
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Retrieve values of request parameter</p>
    METHODS get_request_param_values
      IMPORTING
        iv_param_name    TYPE string
        io_request       TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rt_values) TYPE string_table.

    "! <p class="shorttext synchronized">Get search parameter value(s) from request</p>
    "! @parameter iv_option_name | Optional name for search option<br>
    "!      If not supplied the option name will be the parameter name
    METHODS get_search_parameter
      IMPORTING
        io_request      TYPE REF TO if_adt_rest_request
        iv_param_name   TYPE string
        iv_option_name  TYPE string    OPTIONAL
        if_single_value TYPE abap_bool OPTIONAL
      CHANGING
        ct_option       TYPE zif_sat_ty_object_search=>ty_t_search_option.

    "! <p class="shorttext synchronized">Retrieve value of request parameter</p>
    METHODS get_request_param_value
      IMPORTING
        iv_param_name   TYPE string
        io_request      TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rv_value) TYPE string.

    "! <p class="shorttext synchronized">Post processing for search results</p>
    METHODS post_process_result.

    "! <p class="shorttext synchronized">Convert entry to ADT result</p>
    METHODS convert_to_adt_result
      IMPORTING
        is_result_entry TYPE zif_sat_ty_object_search=>ty_s_search_result
      CHANGING
        cs_result       TYPE zif_sat_ty_adt_types=>ty_s_adt_obj_ref.

  PRIVATE SECTION.
    DATA mo_devclass_util TYPE REF TO lcl_devclass_util.

    "! <p class="shorttext synchronized">Create response</p>
    METHODS create_response
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_s_search_result.
ENDCLASS.


CLASS zcl_sat_adt_res_object_search IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_search_type = iv_search_type.
    mo_search_config = CAST #( zcl_sat_ioc_lookup=>get_instance( iv_contract = 'zif_sat_object_search_config'
                                                                 iv_filter   = |{ mv_search_type }| ) ).
  ENDMETHOD.

  METHOD get.
    CHECK: mv_search_type IS NOT INITIAL,
           mo_search_config IS BOUND.

    get_query_parameter( EXPORTING io_request              = request
                         IMPORTING ev_query                = DATA(lv_query)
                                   es_search_engine_params = DATA(ls_search_engine_params)
                                   et_options              = DATA(lt_options) ).

    TRY.

        DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                                                 iv_contract = 'zif_sat_search_engine' ) ).
        lo_search_engine->search_objects( EXPORTING iv_search_terms         = lv_query
                                                    it_options              = lt_options
                                                    iv_search_type          = mv_search_type
                                                    is_search_engine_params = ls_search_engine_params
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
                                      iv_param_name   = <ls_option>-option
                                      if_single_value = <ls_option>-single
                            CHANGING  ct_option       = et_options ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_response.
    IF mt_query_result IS INITIAL.
      RETURN.
    ENDIF.

    mo_devclass_util = NEW #( ).
    mo_devclass_util->determine_package_hierarchy( mt_query_result ).
    mo_devclass_util->add_packages_to_adt_result( CHANGING search_result = result ).

    post_process_result( ).

    LOOP AT mt_query_result ASSIGNING FIELD-SYMBOL(<ls_search_result>).
      APPEND INITIAL LINE TO result-objects ASSIGNING FIELD-SYMBOL(<ls_object>).

      convert_to_adt_result( EXPORTING is_result_entry = <ls_search_result>
                             CHANGING  cs_result       = <ls_object> ).

    ENDLOOP.
  ENDMETHOD.

  METHOD convert_to_adt_result.
    DATA(ls_object_reference) = zcl_sat_adt_util=>create_adt_uri( iv_type       = is_result_entry-entity_type
                                                                  iv_tadir_type = is_result_entry-tadir_type
                                                                  iv_name       = is_result_entry-object_name
                                                                  iv_name2      = is_result_entry-alt_object_name ).

    cs_result = VALUE #( name        = is_result_entry-object_name
                         alt_name    = is_result_entry-raw_object_name
                         devclass    = is_result_entry-devclass
                         type        = ls_object_reference-type
                         uri         = ls_object_reference-uri
                         parent_uri  = mo_devclass_util->get_package_uri( is_result_entry-devclass )
                         description = is_result_entry-description
                         owner       = is_result_entry-created_by
                         created_on  = is_result_entry-created_date
                         changed_by  = is_result_entry-changed_by
                         changed_on  = is_result_entry-changed_date
                         properties  = VALUE #( ( key = 'API_STATE' value = is_result_entry-api_state ) ) ).
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

  METHOD post_process_result ##NEEDED.
  ENDMETHOD.
ENDCLASS.
