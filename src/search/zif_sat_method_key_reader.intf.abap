"! <p class="shorttext synchronized">Reads method key</p>
INTERFACE zif_sat_method_key_reader
  PUBLIC.

  "! <p class="shorttext synchronized">Returns method key for given class/method name</p>
  METHODS get_method_key
    IMPORTING
      iv_classname   TYPE classname
      iv_method_name TYPE seocpdname
    RETURNING
      VALUE(result)  TYPE seocpdkey.
ENDINTERFACE.
