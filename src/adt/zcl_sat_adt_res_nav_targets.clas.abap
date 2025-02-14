CLASS zcl_sat_adt_res_nav_targets DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_nav_targets TYPE zsat_adt_navigation_target_t.
    DATA mv_object_name TYPE zsat_entity_id.
    DATA mv_object_type TYPE zsat_entity_type.

    "! <p class="shorttext synchronized">Reads Navigation targets for CDS view</p>
    METHODS read_cds_view_targets.
ENDCLASS.


CLASS zcl_sat_adt_res_nav_targets IMPLEMENTATION.
  METHOD get.
    mv_object_name = zcl_sat_adt_res_util=>get_request_param_value(
                         iv_param_name = zif_sat_c_adt_utils=>c_element_info_parameter-name
                         if_mandatory  = abap_true
                         io_request    = request ).
    mv_object_type = zcl_sat_adt_res_util=>get_request_param_value(
                         iv_param_name = zif_sat_c_adt_utils=>c_element_info_parameter-object_type
                         if_mandatory  = abap_true
                         io_request    = request ).

    CASE mv_object_type.

      WHEN zif_sat_c_entity_type=>cds_view.
        read_cds_view_targets( ).

      WHEN zif_sat_c_entity_type=>table.

      WHEN zif_sat_c_entity_type=>view.

    ENDCASE.

    IF mt_nav_targets IS INITIAL.
      RETURN.
    ENDIF.

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_nav_targets_ref_ch( )
                             data            = mt_nav_targets ).
  ENDMETHOD.

  METHOD read_cds_view_targets.
    SELECT SINGLE entityid FROM zsat_p_cdsviewbase
      WHERE entityid = @mv_object_name
         OR ddlname  = @mv_object_name
      INTO @DATA(lv_entity).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    mv_object_name = lv_entity.

    TRY.
        DATA(lo_cds_view) = zcl_sat_cds_view_factory=>read_cds_view( iv_cds_view = mv_object_name ).
        IF lo_cds_view->is_analytics_query( ).
          mt_nav_targets = VALUE #( ( name = 'EXCEL' )
                                    ( name = 'QUERY_MONITOR' ) ).
        ENDIF.
      CATCH zcx_sat_data_read_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
