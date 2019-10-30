"! <p class="shorttext synchronized" lang="en">Search Provider for Extended ABAP Quick Search</p>
"! Provider for Advanced Quicksearch
CLASS zcl_sat_ob_quick_search DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_ob_generic_searcher
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_sat_object_searcher .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_ob_quick_search IMPLEMENTATION.

  METHOD zif_sat_object_searcher~search.
  ENDMETHOD.
ENDCLASS.

