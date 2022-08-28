"! <p class="shorttext synchronized" lang="en">Provides element information for ADT object for a given uri</p>
CLASS zcl_sat_adt_res_eleminfo_byuri DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_element_info
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_parameters
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_eleminfo_byuri IMPLEMENTATION.
  METHOD get_parameters.
    DATA: lv_uri TYPE string.
    TRY.
        io_request->get_uri_query_parameter(
          EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-uri
                    mandatory = abap_true
          IMPORTING value     = lv_uri
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

    TRY.
        zcl_sat_adt_util=>map_uri_to_wb_object(
          EXPORTING
            iv_uri         = lv_uri
          IMPORTING
            ev_object_name = DATA(lv_object_name)
            es_object_type = DATA(ls_object_type)
        ).
        IF ls_object_type-objtype_tr = 'DDLS'.
          mv_object_type = zif_sat_c_entity_type=>cds_view.
        ELSEIF ls_object_type-objtype_tr = 'TABL' AND ls_object_type-subtype_wb = 'DT'.
          mv_object_type = zif_sat_c_entity_type=>table.
        ELSEIF ls_object_type-objtype_tr = 'VIEW'.
          " fallback if view is generated ddic sql view of ddls
          SELECT SINGLE ddlname
            FROM zsat_i_ddldependency
            WHERE viewname = @lv_object_name
            INTO @DATA(lv_ddlname_for_view).
          IF sy-subrc = 0.
            lv_object_name = lv_ddlname_for_view.
            mv_object_type = zif_sat_c_entity_type=>cds_view.
          ELSE.
            mv_object_type = zif_sat_c_entity_type=>view.
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_sat_adt_element_info
            EXPORTING
              textid = zcx_sat_adt_element_info=>object_type_not_supported
              msgv1  = |{ ls_object_type-objtype_tr }/{ ls_object_type-subtype_wb }|.
        ENDIF.

        mv_object_name = lv_object_name.
      catch cx_adt_uri_mapping into data(lx_uri_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_uri_error ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
