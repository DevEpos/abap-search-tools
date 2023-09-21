"! <p class="shorttext synchronized">Object Search Query</p>
INTERFACE zif_sat_object_search_query
  PUBLIC.
  DATA mt_search_term    TYPE zif_sat_ty_object_search=>ty_t_search_term          READ-ONLY.
  DATA mt_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option        READ-ONLY.
  DATA mv_type           TYPE zif_sat_ty_object_search=>ty_search_type            READ-ONLY.
  DATA mv_query          TYPE string                                              READ-ONLY.
  DATA mv_max_rows       TYPE sy-tabix                                            READ-ONLY.
  DATA ms_settings       TYPE zif_sat_ty_object_search=>ty_s_search_engine_params READ-ONLY.

  "! <p class="shorttext synchronized">Has the query options?</p>
  "!
  METHODS has_options
    RETURNING
      VALUE(result) TYPE abap_bool.

  "! <p class="shorttext synchronized">Add search option to query</p>
  "!
  METHODS set_option
    IMPORTING
      is_option TYPE zif_sat_ty_object_search=>ty_s_search_option.

  "! <p class="shorttext synchronized">Get a specific search option</p>
  "!
  METHODS get_option
    IMPORTING
      iv_option        TYPE string
      iv_target        TYPE string OPTIONAL
    RETURNING
      VALUE(rs_option) TYPE zif_sat_ty_object_search=>ty_s_search_option.

  "! <p class="shorttext synchronized">Returns true if query has search terms</p>
  METHODS has_search_terms
    RETURNING
      VALUE(result) TYPE abap_bool.

  "! <p class="shorttext synchronized">Sets max rows to be retrieved by query</p>
  METHODS set_max_rows
    IMPORTING
      iv_max_rows TYPE i.
ENDINTERFACE.
