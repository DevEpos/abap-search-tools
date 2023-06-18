"! <p class="shorttext synchronized">Object Search Resource for CDS Search</p>
CLASS zcl_sat_adt_res_objs_cds DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_object_search
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS post_process_result   REDEFINITION.
    METHODS get_query_parameter   REDEFINITION.
    METHODS convert_to_adt_result REDEFINITION.

  PRIVATE SECTION.
    METHODS read_ddl_sources.

    METHODS set_ddl_positional_uri
      IMPORTING
        is_result_entity TYPE zif_sat_ty_object_search=>ty_s_search_result
      CHANGING
        cs_result        TYPE zif_sat_ty_adt_types=>ty_s_adt_obj_ref.
ENDCLASS.


CLASS zcl_sat_adt_res_objs_cds IMPLEMENTATION.
  METHOD get_query_parameter.
    super->get_query_parameter( EXPORTING io_request              = io_request
                                IMPORTING ev_query                = ev_query
                                          es_search_engine_params = es_search_engine_params
                                          et_options              = et_options ).
  ENDMETHOD.

  METHOD post_process_result.
    " Positional URI is no longer needed for where used starting with NW 7.54
    CHECK sy-saprl < 754.

    " Read sources of all found DDLS search results to get row/column where the name of
    " the entity is starting
    read_ddl_sources( ).
  ENDMETHOD.

  METHOD convert_to_adt_result.
    super->convert_to_adt_result( EXPORTING is_result_entry = is_result_entry
                                  CHANGING  cs_result       = cs_result ).

    cs_result-properties = VALUE #( BASE cs_result-properties
                                    ( key = 'SOURCE_TYPE' value = is_result_entry-custom_field_short1 ) ).
    set_ddl_positional_uri( EXPORTING is_result_entity = is_result_entry
                            CHANGING  cs_result        = cs_result ).
  ENDMETHOD.

  METHOD read_ddl_sources.
    DATA lt_ddlname TYPE ty_lt_ddlname.

    lt_ddlname = VALUE #( FOR res IN mt_query_result
                          WHERE
                          ( tadir_type  = 'DDLS' )
                          ( sign = 'I' option = 'EQ' low = res-alt_object_name ) ).

    SELECT
       FROM ddddlsrc
       FIELDS ddlname,
              source
       WHERE as4local = 'A'
         AND ddlname  IN @lt_ddlname
    INTO CORRESPONDING FIELDS OF TABLE @mt_ddls_source.
  ENDMETHOD.

  METHOD set_ddl_positional_uri.
    ASSIGN mt_ddls_source[ ddlname = is_result_entity-alt_object_name ] TO FIELD-SYMBOL(<ls_source>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    zcl_sat_cds_view_factory=>get_entityname_pos_in_ddlsrc( EXPORTING iv_entity_id = is_result_entity-object_name
                                                                      iv_source    = <ls_source>-source
                                                            IMPORTING ev_column    = DATA(lv_col)
                                                                      ev_row       = DATA(lv_row) ).
    IF lv_col <> -1 AND lv_row <> -1.
      " Adjust ADT URI
      cs_result-uri = |{ cs_result-uri }{ zif_sat_c_adt_utils=>c_ddl_pos_uri_segment }{ lv_row },{ lv_col }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
