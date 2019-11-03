"! <p class="shorttext synchronized" lang="en">Resource for Element info of CDS view</p>
CLASS zcl_sat_adt_res_element_info DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get
        REDEFINITION.
  PROTECTED SECTION.
    DATA mf_basic_info  TYPE abap_bool.
    DATA mv_object_type TYPE zsat_entity_type.
    DATA mv_object_name TYPE zsat_entity_id.

    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        zcx_sat_adt_element_info.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Get request parameters</p>
    "! <p class="shorttext synchronized" lang="en">Loads basic information of element</p>
    METHODS load_basic_element_info
      IMPORTING
        io_response TYPE REF TO if_adt_rest_response
      RAISING
        zcx_sat_adt_element_info.
ENDCLASS.



CLASS zcl_sat_adt_res_element_info IMPLEMENTATION.

  METHOD get.
    get_parameters( request ).

    IF mf_basic_info = abap_true.
      load_basic_element_info( response ).
    ELSE.
      DATA(lo_element_info_reader) = zcl_sat_adt_elinfo_reader_fac=>create_reader(
        io_request  = request
        iv_type     = mv_object_type
        iv_name     = mv_object_name
      ).

      IF lo_element_info_reader IS BOUND.
        lo_element_info_reader->read_element_information( io_rest_response = response ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_parameters.

    TRY.
        io_request->get_uri_query_parameter(
          EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-name
                    mandatory = abap_true
          IMPORTING value     = mv_object_name
        ).
        io_request->get_uri_query_parameter(
          EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-object_type
                    mandatory = abap_true
          IMPORTING value     = mv_object_type
        ).
        io_request->get_uri_query_parameter(
          EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-basic_info
                    default   = abap_false
          IMPORTING value     = mf_basic_info
        ).
      CATCH cx_adt_rest INTO DATA(lx_rest_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_rest_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD load_basic_element_info.
    DATA: ls_entity TYPE zsat_entity.

    DATA(lv_entity) = to_upper( mv_object_name ).

    IF mv_object_type = zif_sat_c_entity_type=>cds_view.
      SELECT SINGLE entityid AS entity_id,
                    rawentityid AS entity_id_raw,
                    createdby AS created_by,
                    description,
                    \_apistate-filtervalue AS api_state,
                    sourcetype AS source_type,
                    ddlsource AS source_code,
                    ddlname AS secondary_entity_id,
                    developmentpackage AS devclass
        FROM zsat_i_cdsentity
        WHERE entityid = @lv_entity
           OR ddlname  = @lv_entity
      INTO CORRESPONDING FIELDS OF @ls_entity.

      CHECK sy-subrc = 0.
    ELSE.
      SELECT SINGLE entity AS entity_id,
                    entityraw AS entity_id_raw,
                    description,
                    createdby AS created_by,
                    developmentpackage AS devclass
        FROM zsat_i_databasetablesandviews
        WHERE entity = @lv_entity
      INTO CORRESPONDING FIELDS OF @ls_entity.

      CHECK sy-subrc = 0.
    ENDIF.

    DATA(ls_adt_object) = zcl_sat_adt_util=>create_adt_uri(
        iv_type   = mv_object_type
        iv_name   = ls_entity-entity_id
        iv_name2  = ls_entity-secondary_entity_id
    ).

    DATA(ls_element_info) = VALUE zsat_adt_element_info(
        name        = ls_entity-entity_id
        raw_name    = ls_entity-entity_id_raw
        type        = ls_adt_object-type
        uri         = ls_adt_object-uri
        owner       = ls_entity-created_by
        package     = ls_entity-devclass
        description = ls_entity-description
    ).

    IF mv_object_type = zif_sat_c_entity_type=>cds_view AND sy-saprl < 754.
*.... Take source_code and create uri so it can be used in Where-Used (<= NW 7.54)
      zcl_sat_cds_view_factory=>get_entityname_pos_in_ddlsrc(
        EXPORTING iv_entity_id = ls_entity-entity_id
                  iv_source    = ls_entity-source_code
        IMPORTING ev_column    = DATA(lv_col)
                  ev_row       = DATA(lv_row)
      ).
      IF lv_col <> -1 AND lv_row <> -1.
        ls_element_info-uri = |{ ls_element_info-uri }{ zif_sat_c_adt_utils=>c_ddl_pos_uri_segment }{ lv_row },{ lv_col }|.
      ENDIF.
    ENDIF.

    IF ls_entity-source_type IS NOT INITIAL.
      ls_element_info-properties = VALUE #( BASE ls_element_info-properties ( key = 'SOURCE_TYPE' value = ls_entity-source_type ) ).
    ENDIF.
    IF ls_entity-api_state IS NOT INITIAL.
      ls_element_info-properties = VALUE #( BASE ls_element_info-properties ( key = 'API_STATE' value = ls_entity-api_state ) ).
    ENDIF.

    TRY.
        io_response->set_body_data(
            content_handler = zcl_sat_adt_ch_factory=>create_generic_eleminfo_res_ch( )
            data            = ls_element_info
        ).
      CATCH cx_adt_rest INTO DATA(lx_rest_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_rest_error ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
