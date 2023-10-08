"! <p class="shorttext synchronized">Resource for Element info of CDS view</p>
CLASS zcl_sat_adt_res_element_info DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.
    DATA mf_basic_info TYPE abap_bool.
    DATA mv_object_type TYPE zsat_entity_type.
    DATA mv_object_name TYPE zsat_entity_id.

    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Get request parameters</p>
    "! <p class="shorttext synchronized">Loads basic information of element</p>
    METHODS load_basic_element_info
      IMPORTING
        io_response TYPE REF TO if_adt_rest_response
      RAISING
        cx_adt_rest.
ENDCLASS.


CLASS zcl_sat_adt_res_element_info IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).

    load_basic_element_info( response ).
  ENDMETHOD.

  METHOD get_parameters.
    io_request->get_uri_query_parameter( EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-name
                                                   mandatory = abap_true
                                         IMPORTING value     = mv_object_name ).
    io_request->get_uri_query_parameter( EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-object_type
                                                   mandatory = abap_true
                                         IMPORTING value     = mv_object_type ).
  ENDMETHOD.

  METHOD load_basic_element_info.
    DATA ls_entity TYPE zsat_entity.

    DATA(lv_entity) = to_upper( mv_object_name ).

    IF mv_object_type = zif_sat_c_entity_type=>cds_view.
      SELECT SINGLE entityid AS entity_id,
                    rawentityid AS entity_id_raw,
                    createdby AS created_by,
                    description,
                    sourcetype AS source_type,
                    ddlsource AS source_code,
                    ddlname AS secondary_entity_id,
                    developmentpackage AS devclass
        FROM zsat_i_cdsentity( p_language = @sy-langu )
        WHERE entityid = @lv_entity
           OR ddlname  = @lv_entity
        INTO CORRESPONDING FIELDS OF @ls_entity.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      SELECT SINGLE entity AS entity_id,
                    entityraw AS entity_id_raw,
                    description,
                    createdby AS created_by,
                    developmentpackage AS devclass
        FROM zsat_i_databasetablesandviews( p_language = @sy-langu )
        WHERE entity = @lv_entity
        INTO CORRESPONDING FIELDS OF @ls_entity.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(ls_adt_object) = zcl_sat_adt_util=>create_adt_uri( iv_type  = mv_object_type
                                                            iv_name  = ls_entity-entity_id
                                                            iv_name2 = ls_entity-secondary_entity_id ).

    DATA(ls_adt_obj_ref) = VALUE zif_sat_ty_adt_types=>ty_s_adt_obj_ref( name        = ls_entity-entity_id
                                                                         alt_name    = ls_entity-entity_id_raw
                                                                         type        = ls_adt_object-type
                                                                         uri         = ls_adt_object-uri
                                                                         owner       = ls_entity-created_by
                                                                         devclass    = ls_entity-devclass
                                                                         description = ls_entity-description ).

    IF mv_object_type = zif_sat_c_entity_type=>cds_view.
      " Take source_code and create uri so it can be used in Where-Used (<= NW 7.54)
      zcl_sat_cds_view_factory=>get_entityname_pos_in_ddlsrc( EXPORTING iv_entity_id = ls_entity-entity_id
                                                                        iv_source    = ls_entity-source_code
                                                              IMPORTING ev_column    = DATA(lv_col)
                                                                        ev_row       = DATA(lv_row) ).
      IF lv_col <> -1 AND lv_row <> -1.
        ls_adt_obj_ref-uri = |{ ls_adt_obj_ref-uri }{ zif_sat_c_adt_utils=>c_ddl_pos_uri_segment }{ lv_row },{ lv_col }|.
      ENDIF.
    ENDIF.

    IF ls_entity-source_type IS NOT INITIAL.
      ls_adt_obj_ref-properties = VALUE #( BASE ls_adt_obj_ref-properties ( key = 'SOURCE_TYPE' value = ls_entity-source_type ) ).
    ENDIF.

    io_response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_adt_obj_ref_res_handler( )
                                data            = ls_adt_obj_ref ).
  ENDMETHOD.
ENDCLASS.
