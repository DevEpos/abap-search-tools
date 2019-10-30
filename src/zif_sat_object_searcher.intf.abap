"! <p class="shorttext synchronized" lang="en">Searcher for Objects</p>
INTERFACE zif_sat_object_searcher
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Search for objects</p>
  "!
  "! @parameter rt_result | <p class="shorttext synchronized" lang="en">List of search results</p>
  "! @raising zcx_sat_object_search | <p class="shorttext synchronized" lang="en">Error during search</p>
  METHODS search
    RETURNING
      VALUE(rt_result) TYPE zsat_entity_t
    RAISING
      zcx_sat_object_search.
ENDINTERFACE.
