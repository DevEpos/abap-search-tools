INTERFACE zif_sat_exception_message
  PUBLIC .


  METHODS print
    IMPORTING
      !iv_msg_type      TYPE sy-msgty DEFAULT 'S'
      !iv_display_type  TYPE sy-msgty DEFAULT 'E'
      !if_to_screen     TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_message) TYPE string .
  METHODS get_message
    RETURNING
      VALUE(result) TYPE string .
ENDINTERFACE.
