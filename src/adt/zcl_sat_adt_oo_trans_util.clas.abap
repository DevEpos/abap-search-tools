"! <p class="shorttext synchronized" lang="en">Utility for Transactions</p>
CLASS zcl_sat_adt_oo_trans_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Starts the query monitor</p>
    "!
    "! Starts the Query Monitor - Transaction RSRT <br/><br/>
    "! <strong>NOTE:</strong><br/>
    "! This does only work from ADT as the transaction does not have any kind
    "! of SET/GET Parameters and the upper case name of the CDS entity name will
    "! be passed as Parameter from ADT to the integrated SAP GUI
    METHODS start_query_monitor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_oo_trans_util IMPLEMENTATION.

  METHOD start_query_monitor.

    DATA(ls_context) = cl_adt_gui_integration_context=>read_context( ).
    CHECK ls_context-parameters IS NOT INITIAL.

    DATA(lr_param_util) = NEW zcl_sat_adt_param_util( ls_context-parameters ).

    cl_adt_gui_integration_context=>initialize_instance( VALUE #( ) ).

    DATA(lt_params) = lr_param_util->get_parameters( ).
*.. Transaction was called via ADT
    IF lt_params IS NOT INITIAL AND
       lines( lt_params ) = 1.

      DATA(lv_entity_id) = VALUE zsat_entity_id( lt_params[ param_id = zif_sat_c_adt_start_params=>entity_id ]-param_value OPTIONAL ).
      IF lv_entity_id IS INITIAL.
        RETURN.
      ENDIF.

      TRY.
          DATA(lo_query_cds) = zcl_sat_cds_view_factory=>read_cds_view( lv_entity_id ).
          DATA(lv_query_ddlview) = lo_query_cds->get_header( )-ddlview.

*.......... Start the Query Monitor
          zcl_sat_query_monitor_util=>open_in_query_monitor(
              iv_query_ddlname = lv_query_ddlview
          ).
        CATCH cx_sy_itab_line_not_found.
        CATCH zcx_sat_data_read_error INTO DATA(lx_read_error).
          lx_read_error->show_message( ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
