CLASS zcl_sat_adt_res_object_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Creates result converter for object search result</p>
    CLASS-METHODS create_result_converter
      IMPORTING
        iv_search_type          TYPE zif_sat_ty_object_search=>ty_search_type
        if_no_package_hierarchy TYPE abap_bool OPTIONAL
        it_query_result         TYPE zif_sat_ty_object_search=>ty_t_search_result
      RETURNING
        VALUE(result)           TYPE REF TO zif_sat_adt_objs_res_converter.

    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_query_result           TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA mf_with_package_hierarchy TYPE abap_bool.
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
  METHOD create_response.
    IF mt_query_result IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_result_converter) = lcl_result_converter_factory=>create_result_converter(
                                    iv_search_type  = ms_query_input-type
                                    it_query_result = mt_query_result ).
    result = lo_result_converter->zif_sat_adt_objs_res_converter~convert( ).
  ENDMETHOD.

  METHOD create_result_converter.
    DATA(lo_result_converter) = lcl_result_converter_factory=>create_result_converter(
                                    iv_search_type  = iv_search_type
                                    it_query_result = it_query_result ).
    lo_result_converter->set_no_package_hiearchy( if_no_package_hierarchy ).
    result = CAST #( lo_result_converter ).
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

  METHOD get_search_terms.
    LOOP AT ms_query_input-fields REFERENCE INTO DATA(lr_field) WHERE values IS NOT INITIAL.
      result = VALUE #( BASE result
                        ( target = lr_field->name
                          values = VALUE #( FOR val IN lr_field->values ( low = val ) ) ) ) ##OPERATOR[SIGN].
    ENDLOOP.
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
                        get_all                  = ms_query_input-no_row_limit
                        custom_options           = ms_query_input-custom_options )
          IMPORTING et_results              = mt_query_result ).

      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_object_search
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text(
                                 exception = COND #( WHEN lx_search_error->previous IS BOUND
                                                     THEN lx_search_error->previous
                                                     ELSE lx_search_error ) ).
    ENDTRY.

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_search_result_ch( )
                             data            = create_response( ) ).
  ENDMETHOD.
ENDCLASS.
