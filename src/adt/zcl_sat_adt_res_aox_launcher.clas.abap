"! <p class="shorttext synchronized">Resource to retrieve Analysis for Office Launcher for Query</p>
CLASS zcl_sat_adt_res_aox_launcher DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_aox_launcher IMPLEMENTATION.
  METHOD get.
    DATA lv_entity_id TYPE zsat_cds_view_name.

    request->get_uri_query_parameter( EXPORTING name      = 'objectName'
                                                mandatory = abap_true
                                      IMPORTING value     = lv_entity_id ).

    " Retrieve DDIC SQL View for data definition
    SELECT SINGLE viewname
      FROM zsat_i_cdsentity
      WHERE ddlname = @lv_entity_id
         OR entityid = @lv_entity_id
      INTO @DATA(lv_ddlview).
    IF sy-subrc = 0.
      DATA(lv_launcher_file) = zcl_sat_query_monitor_util=>create_sapaox_launcher( lv_ddlview ).
      IF lv_launcher_file IS INITIAL.
        RETURN.
      ENDIF.

      response->set_body_data( content_handler = NEW lcl_dummy_content_handler( lv_launcher_file )
                               data            = lv_launcher_file ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
