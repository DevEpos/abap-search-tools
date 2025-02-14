"! <p class="shorttext synchronized">Logger for Object Search</p>
CLASS zcl_sat_os_logger DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_query TYPE REF TO zif_sat_object_search_query.

    METHODS start_timer.
    METHODS stop_timer.

    METHODS set_error
      IMPORTING
        iv_error TYPE string.

    METHODS set_join_count
      IMPORTING
        iv_value TYPE i.

    METHODS write_log.

    METHODS set_selected_entries
      IMPORTING
        iv_value TYPE i.

    METHODS set_distinct_active
      IMPORTING
        if_active TYPE abap_bool.

    METHODS set_group_by_active
      IMPORTING
        if_active TYPE abap_bool.

    METHODS add_filter_count
      IMPORTING
        iv_value TYPE i.

    METHODS add_filter_value_count
      IMPORTING
        iv_value TYPE i.

    METHODS set_select_stmnt
      IMPORTING
        iv_value TYPE string.

    METHODS set_base_table
      IMPORTING
        iv_value TYPE tabname.

    METHODS set_query_hash
      IMPORTING
        io_query           TYPE REF TO zif_sat_object_search_query
        is_search_settings TYPE zif_sat_ty_object_search=>ty_s_search_engine_params.

  PRIVATE SECTION.
    DATA ms_log_entry TYPE zsatsearchlog.
    DATA mo_timer TYPE REF TO if_abap_runtime.
    DATA mf_log_active TYPE abap_bool.

ENDCLASS.


CLASS zcl_sat_os_logger IMPLEMENTATION.
  METHOD constructor.
    mf_log_active = zcl_sat_log=>is_search_log_active( ).
    ms_log_entry-search_type = io_query->mv_type.
    ms_log_entry-max_entries = io_query->mv_max_rows.
  ENDMETHOD.

  METHOD start_timer.
    CHECK mf_log_active = abap_true.

    mo_timer = cl_abap_runtime=>create_hr_timer( ).
    mo_timer->get_runtime( ).
  ENDMETHOD.

  METHOD stop_timer.
    CHECK mf_log_active = abap_true.
    CHECK mo_timer IS BOUND.

    DATA(lv_duration) = mo_timer->get_runtime( ).

    IF ms_log_entry-select_duration_ms IS INITIAL.
      ms_log_entry-select_duration_ms = lv_duration / 1000.
    ELSE.
      ms_log_entry-post_search_duration_ms = lv_duration / 1000.
    ENDIF.
  ENDMETHOD.

  METHOD set_error.
    ms_log_entry-error_msg = iv_error.
  ENDMETHOD.

  METHOD add_filter_count.
    ms_log_entry-filter_count = ms_log_entry-filter_count + iv_value.
  ENDMETHOD.

  METHOD add_filter_value_count.
    ms_log_entry-filter_value_count = ms_log_entry-filter_value_count + iv_value.
  ENDMETHOD.

  METHOD set_distinct_active.
    ms_log_entry-distinct_active = if_active.
  ENDMETHOD.

  METHOD set_group_by_active.
    ms_log_entry-group_by_active = if_active.
  ENDMETHOD.

  METHOD set_join_count.
    ms_log_entry-joins = iv_value.
  ENDMETHOD.

  METHOD set_selected_entries.
    ms_log_entry-selected_entries = iv_value.
  ENDMETHOD.

  METHOD set_select_stmnt.
    ms_log_entry-select_stmnt = iv_value.
  ENDMETHOD.

  METHOD set_query_hash.
    CHECK mf_log_active = abap_true.

    CALL TRANSFORMATION id
         OPTIONS initial_components = 'suppress'
         SOURCE terms    = io_query->mt_search_term
                max_rows = io_query->mv_max_rows
                options  = io_query->mt_search_options
                type     = io_query->mv_type
                settings = is_search_settings
         RESULT XML DATA(lv_query).

    TRY.
        cl_abap_message_digest=>calculate_hash_for_raw( EXPORTING if_data       = lv_query
                                                        IMPORTING ef_hashstring = DATA(lv_hashed_value) ).
        ms_log_entry-query_hash = lv_hashed_value.
      CATCH cx_abap_message_digest.
        ms_log_entry-query_hash = '-'.
    ENDTRY.
  ENDMETHOD.

  METHOD write_log.
    CHECK mf_log_active = abap_true.
    zcl_sat_log=>write_log_entry( ms_log_entry ).
  ENDMETHOD.

  METHOD set_base_table.
    ms_log_entry-base_table = iv_value.
  ENDMETHOD.
ENDCLASS.
