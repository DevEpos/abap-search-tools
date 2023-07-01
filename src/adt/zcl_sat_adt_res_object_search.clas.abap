CLASS zcl_sat_adt_res_object_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_query_result           TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA mf_with_package_hierarchy TYPE abap_bool.
    DATA mo_search_config          TYPE REF TO zif_sat_object_search_config.
    DATA ms_query_input            TYPE zif_sat_ty_adt_types=>ty_s_query_input.

    "! <p class="shorttext synchronized">Create response</p>
    METHODS create_response
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_s_search_result.

    METHODS get_search_terms
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_search_term.

    METHODS get_options
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_search_option.
ENDCLASS.


CLASS zcl_sat_adt_res_object_search IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = zcl_sat_adt_ch_factory=>create_query_input_ch( )
                            IMPORTING data            = ms_query_input ).

    mo_search_config = CAST #( zcl_sat_ioc_lookup=>get_instance( iv_contract = 'zif_sat_object_search_config'
                                                                 iv_filter   = |{ ms_query_input-type }| ) ).

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

  METHOD create_response.
    IF mt_query_result IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_result_converter) = lcl_result_converter_factory=>create_result_converter(
                                    iv_search_type  = ms_query_input-type
                                    it_query_result = mt_query_result ).
    result = lo_result_converter->convert( ).
  ENDMETHOD.

  METHOD get_search_terms.
    LOOP AT ms_query_input-fields REFERENCE INTO DATA(lr_field) WHERE values IS NOT INITIAL.
      result = VALUE #( BASE result
                        ( target = lr_field->name
                          values = VALUE #( FOR val IN lr_field->values ( low = val ) ) ) ) ##OPERATOR[SIGN].
    ENDLOOP.
  ENDMETHOD.

  METHOD get_options.
    LOOP AT ms_query_input-fields REFERENCE INTO DATA(lr_field) WHERE filters IS NOT INITIAL.

      LOOP AT lr_field->filters REFERENCE INTO DATA(lr_filter) WHERE values IS NOT INITIAL.
        result = VALUE #( BASE result
                          ( target      = lr_field->name
                            option      = lr_filter->name
                            value_range = VALUE #( FOR filt IN lr_filter->values ( low = filt ) ) ) ) ##OPERATOR[SIGN].
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
