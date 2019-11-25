"! <p class="shorttext synchronized" lang="en">Parser for object search query</p>
INTERFACE zif_sat_object_query_parser
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Parses the given search string</p>
  "! The given string is processed to extract additional search parameters
  "!
  "! @parameter iv_search_query | <p class="shorttext synchronized" lang="en">The query string to be parsed</p>
  "! @parameter ro_query | <p class="shorttext synchronized" lang="en">The created query instance</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Query exception if errors occurred</p>
  METHODS parse_query
    IMPORTING
      iv_search_query         TYPE string
    RETURNING
      VALUE(ro_query)         TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized" lang="en">Creates query from string and options</p>
  "!
  "! @parameter iv_search_terms | <p class="shorttext synchronized" lang="en">A string with search terms</p>
  "! @parameter it_options | <p class="shorttext synchronized" lang="en">A table of search options</p>
  "! @parameter ro_query | <p class="shorttext synchronized" lang="en">The created query instance</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Query exception if errors occurred</p>
  METHODS create_query
    IMPORTING
      iv_search_terms         TYPE string OPTIONAL
      it_options              TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL
    RETURNING
      VALUE(ro_query)         TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
