"! <p class="shorttext synchronized">Options/Constants for Message Search</p>
INTERFACE zif_sat_c_os_mess_options
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_search_fields,
      message_text_input_key     TYPE string VALUE 'messageText',
      message_text_input_label   TYPE string VALUE '&Message Short Text',
      message_filter_input_key   TYPE string VALUE 'messageFilter',
      message_filter_input_label TYPE string VALUE 'M&essage Filter',
    END OF c_search_fields.

  CONSTANTS:
    BEGIN OF c_filter_key,
      self_explanatory TYPE string VALUE 'selfexpl',
    END OF c_filter_key.
ENDINTERFACE.
