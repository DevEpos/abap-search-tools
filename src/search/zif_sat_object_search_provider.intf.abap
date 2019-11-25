"! <p class="shorttext synchronized" lang="en">Search Provider for Object search</p>
INTERFACE zif_sat_object_search_provider
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Search for objects</p>
  "!
  "! @parameter io_query | <p class="shorttext synchronized" lang="en">Instance of object search query</p>
  "! @parameter is_search_engine_params | <p class="shorttext synchronized" lang="en">Additional Search engine options</p>
  "! @parameter et_result | <p class="shorttext synchronized" lang="en">List of search results</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Error during search</p>
  METHODS search
    IMPORTING
      io_query                TYPE REF TO zif_sat_object_search_query
      is_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params OPTIONAL
    EXPORTING
      et_result               TYPE zsat_entity_t
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
