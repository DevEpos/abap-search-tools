"! <p class="shorttext synchronized">Interface for Search Engine</p>
INTERFACE zif_sat_search_engine
  PUBLIC.

  "! <p class="shorttext synchronized">Parses the given search string</p>
  "! The given string is processed to extract additional search parameters
  "!
  "! @parameter iv_search_query       | The query string to be parsed
  "! @parameter iv_search_type        | The search type
  "! @parameter ro_query              | The created query instance
  "! @raising   zcx_sat_object_search | Query exception if errors occurred
  METHODS parse_query
    IMPORTING
      iv_search_query TYPE string
      iv_search_type  TYPE zif_sat_ty_object_search=>ty_search_type
    RETURNING
      VALUE(ro_query) TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized">Creates query from string and options</p>
  "!
  "! @parameter iv_search_type        | The search type
  "! @parameter it_options            | A table of search options
  "! @parameter it_search_terms       | A list of search terms for each target
  "! @parameter ro_query              | The created query instance
  "! @raising   zcx_sat_object_search | Query exception if errors occurred
  METHODS create_query
    IMPORTING
      iv_search_type  TYPE zif_sat_ty_object_search=>ty_search_type
      it_options      TYPE zif_sat_ty_object_search=>ty_t_search_option OPTIONAL
      it_search_terms TYPE zif_sat_ty_object_search=>ty_t_search_term   OPTIONAL
    RETURNING
      VALUE(ro_query) TYPE REF TO zif_sat_object_search_query
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized">Searches for objects for the given query</p>
  "!
  "! @parameter io_query                | The search query instance
  "! @parameter is_search_engine_params | Optional parameters for the search
  "! @parameter et_results              | The found search results
  "! @raising   zcx_sat_object_search   | Query exception if errors occurred
  METHODS search_objects_by_query
    IMPORTING
      io_query                TYPE REF TO zif_sat_object_search_query
      is_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params OPTIONAL
    EXPORTING
      et_results              TYPE zif_sat_ty_object_search=>ty_t_search_result
    RAISING
      zcx_sat_object_search.

  "! <p class="shorttext synchronized">Searches for objects for the given query</p>
  "!
  "! @parameter iv_search_type          | The search type
  "! @parameter it_search_terms         | A list of search terms for a target
  "! @parameter it_options              | A table of search options
  "! @parameter iv_max_rows             | Max rows to be returned
  "! @parameter is_search_engine_params | Optional parameters for the search
  "! @parameter et_results              | The found search results
  "! @raising   zcx_sat_object_search   | Query exception if errors occurred
  METHODS search_objects
    IMPORTING
      iv_search_type          TYPE zif_sat_ty_object_search=>ty_search_type
      it_search_terms         TYPE zif_sat_ty_object_search=>ty_t_search_term          OPTIONAL
      it_options              TYPE zif_sat_ty_object_search=>ty_t_search_option        OPTIONAL
      iv_max_rows             TYPE i                                                   DEFAULT 500
      is_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params OPTIONAL
    EXPORTING
      et_results              TYPE zif_sat_ty_object_search=>ty_t_search_result
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
