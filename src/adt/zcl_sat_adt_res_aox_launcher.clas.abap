CLASS zcl_sat_adt_res_aox_launcher DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_aox_launcher IMPLEMENTATION.

  METHOD get.
    DATA: lv_entity_id TYPE zsat_cds_view_name.

    request->get_uri_query_parameter(
      EXPORTING name      = 'objectName'
                mandatory = abap_true
      IMPORTING value     = lv_entity_id
    ).

*... Read Query cds view to get DDL view of query and underlying Cube view
    TRY.
        DATA(lo_cds_view) = zcl_sat_cds_view_factory=>read_cds_view( iv_cds_view = lv_entity_id ).
        DATA(lv_query_ddl_view) = lo_cds_view->get_header( )-ddlview.

        DATA(lv_launcher_file) = zcl_sat_query_monitor_util=>create_sapaox_launcher( lv_query_ddl_view ).
        CHECK lv_launcher_file IS NOT INITIAL.

        response->set_body_data(
            content_handler = NEW lcl_dummy_content_handler( lv_launcher_file )
            data            = lv_launcher_file
        ).

      CATCH zcx_sat_data_read_error INTO DATA(lx_cds_read_error).

    ENDTRY.
  ENDMETHOD.

ENDCLASS.
