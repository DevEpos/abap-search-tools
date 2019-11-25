"! <p class="shorttext synchronized" lang="en">Interface for Search Engine</p>
INTERFACE zif_sat_search_engine
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Parses the given search string</p>
  "! The given string is processed to extract additional search parameters
  "!
  "! @parameter iv_search_query | <p class="shorttext synchronized" lang="en">The query string to be parsed</p>
  "! @parameter iv_search_type | <p class="shorttext synchronized" lang="en">The search type</p>
  "! @parameter ro_query | <p class="shorttext synchronized" lang="en">The created query instance</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Query exception if errors occurred</p>
  METHODS parse_query
    IMPORTING
      iv_search_query TYPE string
      iv_search_type  TYPE zif_sat_ty_object_search=>ty_search_type
    RETURNING
      VALUE(ro_query) TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized" lang="en">Creates query from string and options</p>
  "!
  "! @parameter iv_search_terms | <p class="shorttext synchronized" lang="en">A string with search terms</p>
  "! @parameter iv_search_type | <p class="shorttext synchronized" lang="en">The search type</p>
  "! @parameter it_options | <p class="shorttext synchronized" lang="en">A table of search options</p>
  "! @parameter ro_query | <p class="shorttext synchronized" lang="en">The created query instance</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Query exception if errors occurred</p>
  METHODS create_query
    IMPORTING
      iv_search_terms TYPE string OPTIONAL
      iv_search_type  TYPE zif_sat_ty_object_search=>ty_search_type
      it_options      TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL
    RETURNING
      VALUE(ro_query) TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized" lang="en">Searches for objects for the given query</p>
  "!
  "! @parameter io_query | <p class="shorttext synchronized" lang="en">The search query instance</p>
  "! @parameter is_search_engine_params | <p class="shorttext synchronized" lang="en">Optional parameters for the search</p>
  "! @parameter et_results | <p class="shorttext synchronized" lang="en">The found search results</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Query exception if errors occurred</p>
  METHODS search_objects_by_query
    IMPORTING
      io_query                TYPE REF TO zif_sat_object_search_query
      is_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params OPTIONAL
    EXPORTING
      et_results              TYPE zsat_entity_t
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized" lang="en">Searches for objects for the given query</p>
  "!
  "! @parameter iv_search_terms | <p class="shorttext synchronized" lang="en">A String with search terms</p>
  "! @parameter it_options | <p class="shorttext synchronized" lang="en">A table of search options</p>
  "! @parameter iv_search_type | <p class="shorttext synchronized" lang="en">The search type</p>
  "! @parameter is_search_engine_params | <p class="shorttext synchronized" lang="en">Optional parameters for the search</p>
  "! @parameter et_results | <p class="shorttext synchronized" lang="en">The found search results</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Query exception if errors occurred</p>
  METHODS search_objects
    IMPORTING
      iv_search_terms         TYPE string OPTIONAL
      it_options              TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL
      iv_search_type          TYPE zif_sat_ty_object_search=>ty_search_type
      is_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params OPTIONAL
    EXPORTING
      et_results              TYPE zsat_entity_t
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
