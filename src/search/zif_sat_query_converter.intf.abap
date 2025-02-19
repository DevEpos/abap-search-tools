"! <p class="shorttext synchronized">Converter for Object Search Query</p>
INTERFACE zif_sat_query_converter
  PUBLIC.

  "! <p class="shorttext synchronized">Converts value of search option (if necessary)</p>
  "!
  METHODS convert_value
    IMPORTING
      iv_sign   TYPE ddsign
      iv_sign2  TYPE ddsign
      iv_option TYPE string
      iv_target TYPE string OPTIONAL
    EXPORTING
      es_range  TYPE zif_sat_ty_object_search=>ty_s_value_range
    CHANGING
      cv_value  TYPE string
      cv_value2 TYPE string OPTIONAL
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
