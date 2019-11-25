"! <p class="shorttext synchronized" lang="en">Validates Search Query</p>
INTERFACE zif_sat_query_validator
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Validates the given option</p>
  "!
  "! @parameter iv_option | <p class="shorttext synchronized" lang="en">Name of a query option</p>
  "! @parameter iv_value | <p class="shorttext synchronized" lang="en">Value for the given option</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Error if validation failed</p>
  METHODS validate_option
    IMPORTING
      iv_option TYPE string
      iv_value  TYPE string
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized" lang="en">Checks integrity of search options</p>
  "! This method should be called after the initial validation of each option
  "! by itself. It performs integrity checks between the search options.
  "!
  "! @parameter ct_options | <p class="shorttext synchronized" lang="en">A table of search options</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Error during checks occurred</p>
  METHODS check_option_integrity DEFAULT IGNORE
    CHANGING
      ct_options TYPE zif_sat_ty_object_search=>ty_t_search_option
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
