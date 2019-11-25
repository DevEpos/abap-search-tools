"! <p class="shorttext synchronized" lang="en">Object Search Query</p>
INTERFACE zif_sat_object_search_query
  PUBLIC .
  DATA mt_search_term TYPE RANGE OF string READ-ONLY.
  DATA mv_type TYPE zif_sat_ty_object_search=>ty_search_type READ-ONLY.
  DATA mv_query TYPE string READ-ONLY.
  DATA mv_max_rows TYPE sy-tabix READ-ONLY.
  DATA mt_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option READ-ONLY.

  "! <p class="shorttext synchronized" lang="en">Has the query options?</p>
  "!
  METHODS has_options
    RETURNING
      VALUE(result) TYPE abap_bool.
  "! <p class="shorttext synchronized" lang="en">Add search option to query</p>
  "!
  METHODS set_option
    IMPORTING
      is_option TYPE zif_sat_ty_object_search=>ty_s_search_option.
  "! <p class="shorttext synchronized" lang="en">Get a specific search option</p>
  "!
  METHODS get_option
    IMPORTING
      iv_option        TYPE string
    RETURNING
      VALUE(rs_option) TYPE zif_sat_ty_object_search=>ty_s_search_option.
  "! <p class="shorttext synchronized" lang="en">Returns true if query has search terms</p>
  METHODS has_search_terms
    RETURNING
      VALUE(result) TYPE abap_bool.
ENDINTERFACE.
