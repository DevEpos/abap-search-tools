"! <p class="shorttext synchronized" lang="en">Converter for Object Search Query</p>
INTERFACE zif_sat_query_converter
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Converts value of search option (if necessary)</p>
  "!
  "! @parameter iv_option | <p class="shorttext synchronized" lang="en">Id of a search option</p>
  "! @parameter cv_value | <p class="shorttext synchronized" lang="en">Value to be converted</p>
  "! @parameter cv_value2 | <p class="shorttext synchronized" lang="en">Additional value for key/value options</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Error during conversion occurred</p>
  METHODS convert_value
    IMPORTING
      iv_option TYPE string
    CHANGING
      cv_value  TYPE string
      cv_value2 TYPE string OPTIONAL
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
