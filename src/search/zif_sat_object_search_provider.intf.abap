"! <p class="shorttext synchronized">Search Provider for Object search</p>
INTERFACE zif_sat_object_search_provider
  PUBLIC.

  "! <p class="shorttext synchronized">Search for objects</p>
  "!
  "! @parameter io_query                | Instance of object search query
  "! @parameter is_search_engine_params | Additional Search engine options
  "! @parameter et_result               | List of search results
  "! @raising   zcx_sat_object_search   | Error during search
  METHODS search
    IMPORTING
      io_query                TYPE REF TO zif_sat_object_search_query
      is_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params OPTIONAL
    EXPORTING
      et_result               TYPE zif_sat_ty_object_search=>ty_t_search_result
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
