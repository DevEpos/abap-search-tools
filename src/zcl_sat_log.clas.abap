"! <p class="shorttext synchronized">Logging Util for Search and Analysis Tools</p>
CLASS zcl_sat_log DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Activates logging for Object Search
    CLASS-METHODS activate_search_log.
    "! Deactivates logging for Object Search
    CLASS-METHODS deactivate_search_log.
    "! Delete all entries from the search log
    CLASS-METHODS clear_search_log.

    "! Returns 'X' if the object search log is activated
    CLASS-METHODS is_search_log_active
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Writes the given log entry to the database
    CLASS-METHODS write_log_entry
      IMPORTING
        is_log_data TYPE zsatsearchlog.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_log_id,
        search TYPE c LENGTH 30 VALUE 'search',
      END OF c_log_id.
ENDCLASS.


CLASS zcl_sat_log IMPLEMENTATION.
  METHOD activate_search_log.
    DATA(ls_log_status) = VALUE zsatlogstatus( log_id = c_log_id-search active = abap_true ).
    MODIFY zsatlogstatus FROM ls_log_status.
    COMMIT WORK.
  ENDMETHOD.

  METHOD deactivate_search_log.
    DATA(ls_log_status) = VALUE zsatlogstatus( log_id = c_log_id-search active = abap_false ).
    MODIFY zsatlogstatus FROM ls_log_status.
    COMMIT WORK.
  ENDMETHOD.

  METHOD is_search_log_active.
    SELECT SINGLE @abap_true FROM zsatlogstatus
      WHERE log_id = @c_log_id-search
        AND active = @abap_true
      INTO @result.
  ENDMETHOD.

  METHOD write_log_entry.
    DATA(ls_log_entry) = is_log_data.
    TRY.
        ls_log_entry-id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error ##NEEDED.
        RETURN.
    ENDTRY.

    ls_log_entry-created_date = sy-datum.
    ls_log_entry-created_time = sy-timlo.
    ls_log_entry-system_id    = sy-sysid.

    TRY.
        INSERT zsatsearchlog FROM ls_log_entry.
        COMMIT WORK.
      CATCH cx_sy_open_sql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD clear_search_log.
    DELETE FROM zsatsearchlog.
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.
