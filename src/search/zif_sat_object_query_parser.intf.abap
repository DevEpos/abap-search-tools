"! <p class="shorttext synchronized">Parser for object search query</p>
INTERFACE zif_sat_object_query_parser
  PUBLIC.

  "! <p class="shorttext synchronized">Parses the given search string</p>
  "! The given string is processed to extract additional search parameters
  "!
  "! @parameter iv_search_query       | The query string to be parsed
  "! @parameter ro_query              | The created query instance
  "! @raising   zcx_sat_object_search | Query exception if errors occurred
  METHODS parse_query
    IMPORTING
      iv_search_query TYPE string
    RETURNING
      VALUE(ro_query) TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized">Creates query from string and options</p>
  "!
  "! @parameter iv_search_terms         | A string with search terms
  "! @parameter iv_sub_obj_search_terms | Search terms for sub object search
  "! @parameter it_options              | A table of search options
  "! @parameter ro_query                | The created query instance
  "! @raising   zcx_sat_object_search   | Query exception if errors occurred
  METHODS create_query
    IMPORTING
      iv_search_terms         TYPE string                                       OPTIONAL
      iv_sub_obj_search_terms TYPE string                                       OPTIONAL
      it_options              TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL
    RETURNING
      VALUE(ro_query)         TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
