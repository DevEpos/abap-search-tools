"! <p class="shorttext synchronized">Validates Search Query</p>
INTERFACE zif_sat_query_validator
  PUBLIC.

  "! <p class="shorttext synchronized">Validates the given option</p>
  "!
  "! @parameter iv_option             | <p class="shorttext synchronized">Name of a query option</p>
  "! @parameter is_content_assist     | <p class="shorttext synchronized">Content Assist configuration of the filter option</p>
  "! @parameter iv_target             | <p class="shorttext synchronized">Target of option (object/sub object)</p>
  "! @parameter iv_value              | <p class="shorttext synchronized">Value for the given option</p>
  "! @parameter iv_value2             | <p class="shorttext synchronized">Additional value for key/value options</p>
  "! @raising   zcx_sat_object_search | <p class="shorttext synchronized">Error if validation failed</p>
  METHODS validate_option
    IMPORTING
      iv_option         TYPE string
      is_content_assist TYPE zif_sat_ty_object_search=>ty_option_content_assist
      iv_target         TYPE string OPTIONAL
      iv_value          TYPE string
      iv_value2         TYPE string OPTIONAL
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized">Checks integrity of search options</p>
  "! This method should be called after the initial validation of each option
  "! by itself. It performs integrity checks between the search options.
  "!
  "! @parameter ct_options            | <p class="shorttext synchronized">A table of search options</p>
  "! @raising   zcx_sat_object_search | <p class="shorttext synchronized">Error during checks occurred</p>
  METHODS check_option_integrity DEFAULT IGNORE
    CHANGING
      ct_options TYPE zif_sat_ty_object_search=>ty_t_search_option
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
