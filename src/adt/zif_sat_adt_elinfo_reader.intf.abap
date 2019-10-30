"! <p class="shorttext synchronized" lang="en">Database Entity Element information reader</p>
INTERFACE zif_sat_adt_elinfo_reader
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Reads element information</p>
  METHODS read_element_information
    IMPORTING
      io_rest_response TYPE REF TO if_adt_rest_response
    RAISING
      zcx_sat_adt_element_info.
ENDINTERFACE.
