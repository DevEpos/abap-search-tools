"! <p class="shorttext synchronized">Helper class for Message handling</p>
CLASS zcl_sat_message_helper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Prints exception message</p>
    CLASS-METHODS print_exc_message
      IMPORTING
        is_textid         TYPE scx_t100key
        if_to_screen      TYPE abap_bool  DEFAULT abap_true
        ir_previous       TYPE REF TO cx_root
        iv_display_type   TYPE syst_msgty DEFAULT 'E'
        iv_message_type   TYPE syst_msgty DEFAULT 'E'
        ir_exc_message    TYPE REF TO zif_sat_exception_message
        iv_msgv1          TYPE sy-msgv1   OPTIONAL
        iv_msgv2          TYPE sy-msgv2   OPTIONAL
        iv_msgv3          TYPE sy-msgv3   OPTIONAL
        iv_msgv4          TYPE sy-msgv4   OPTIONAL
      RETURNING
        VALUE(rv_message) TYPE string.

    "! <p class="shorttext synchronized">Prints the currently stored system message</p>
    CLASS-METHODS print_system_message.

    "! <p class="shorttext synchronized">Splits string for MESSAGE command</p>
    CLASS-METHODS split_string_for_message
      IMPORTING
        iv_string TYPE string
      EXPORTING
        ev_msgv1  TYPE sy-msgv1
        ev_msgv2  TYPE sy-msgv2
        ev_msgv3  TYPE sy-msgv3
        ev_msgv4  TYPE sy-msgv4.
ENDCLASS.


CLASS zcl_sat_message_helper IMPLEMENTATION.
  METHOD print_system_message.
    MESSAGE ID sy-msgid
            TYPE   sy-msgty
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.

  METHOD print_exc_message.
    " TODO: parameter IR_EXC_MESSAGE is never used (ABAP cleaner)

    IF is_textid-msgid = 'SY' AND is_textid-msgno = 530.

      " try to print message of previous exception
      IF ir_previous IS BOUND.
        TRY.
            DATA(lr_exception_message) = CAST zif_sat_exception_message( ir_previous ).
            IF if_to_screen = abap_true.
              rv_message = lr_exception_message->print( iv_msg_type     = iv_message_type
                                                        iv_display_type = iv_display_type
                                                        if_to_screen    = if_to_screen ).
            ELSE.
              rv_message = lr_exception_message->get_message( ).
            ENDIF.
          CATCH cx_sy_move_cast_error.
            " Return message from non db browser exception
            IF if_to_screen = abap_true.
              MESSAGE ir_previous->get_text( ) TYPE iv_message_type DISPLAY LIKE iv_display_type.
            ELSE.
              rv_message = ir_previous->get_text( ).
            ENDIF.
        ENDTRY.
      ENDIF.
    ELSE.

      IF if_to_screen = abap_true.
        MESSAGE ID is_textid-msgid
                TYPE   iv_message_type
                NUMBER is_textid-msgno
                WITH   iv_msgv1
                       iv_msgv2
                       iv_msgv3
                       iv_msgv4
                DISPLAY LIKE iv_display_type.
      ELSE.
        MESSAGE ID is_textid-msgid
                TYPE   'E'
                NUMBER is_textid-msgno
                WITH   iv_msgv1
                       iv_msgv2
                       iv_msgv3
                       iv_msgv4
                INTO rv_message.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD split_string_for_message.
    DATA lv_off TYPE i.
    DATA lv_string TYPE string.

    lv_string = iv_string.

    ev_msgv1 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv2 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv3 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv4 = lv_string.

    IF strlen( lv_string ) > 50.
      FIND ALL OCCURRENCES OF REGEX '.\s.' IN SECTION LENGTH 47 OF ev_msgv4 MATCH OFFSET lv_off.
      IF sy-subrc = 0.
        lv_off = lv_off + 1.
        ev_msgv4 = ev_msgv4(lv_off).

        ev_msgv4 = |{ ev_msgv4 }...|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
