"! <p class="shorttext synchronized">Utility for Transactions</p>
CLASS zcl_sat_adt_oo_trans_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Starts the query monitor</p>
    "!
    "! Starts the Query Monitor - Transaction RSRT <br/><br/>
    "! <strong>NOTE:</strong><br/>
    "! This does only work from ADT as the transaction does not have any kind
    "! of SET/GET Parameters and the upper case name of the CDS entity name will
    "! be passed as Parameter from ADT to the integrated SAP GUI
    METHODS start_query_monitor.
ENDCLASS.


CLASS zcl_sat_adt_oo_trans_util IMPLEMENTATION.
  METHOD start_query_monitor.
    DATA(ls_context) = cl_adt_gui_integration_context=>read_context( ).
    IF ls_context-parameters IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lr_param_util) = NEW zcl_sat_adt_param_util( ls_context-parameters ).

    cl_adt_gui_integration_context=>initialize_instance( VALUE #( ) ).

    DATA(lt_params) = lr_param_util->get_parameters( ).
    " Transaction was called via ADT
    IF    lt_params          IS INITIAL
       OR lines( lt_params ) <> 1.
      RETURN.
    ENDIF.

    DATA(lv_entity_id) = VALUE zsat_entity_id( lt_params[ param_id = zif_sat_c_adt_start_params=>entity_id ]-param_value OPTIONAL ).
    IF lv_entity_id IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE viewname
      FROM zsat_p_cdsviewbase
      WHERE ddlname = @lv_entity_id
         OR entityid = @lv_entity_id
      INTO @DATA(lv_ddic_view).
    IF sy-subrc = 0.
    ENDIF.

    " Start the Query Monitor
    zcl_sat_query_monitor_util=>open_in_query_monitor( iv_query_ddlname = lv_ddic_view ).
  ENDMETHOD.
ENDCLASS.
