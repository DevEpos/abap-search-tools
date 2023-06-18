"! <p class="shorttext synchronized">Query for Object Search</p>
CLASS zcl_sat_object_search_query DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_query.
    INTERFACES zif_sat_c_object_search.

    ALIASES mt_search_term     FOR zif_sat_object_search_query~mt_search_term.
    ALIASES mt_sub_search_term FOR zif_sat_object_search_query~mt_sub_search_term.
    ALIASES mv_type            FOR zif_sat_object_search_query~mv_type.
    ALIASES mv_query           FOR zif_sat_object_search_query~mv_query.
    ALIASES mv_max_rows        FOR zif_sat_object_search_query~mv_max_rows.
    ALIASES mt_search_options  FOR zif_sat_object_search_query~mt_search_options.

    "! <p class="shorttext synchronized">Creates new Search Query</p>
    "!
    "! @parameter iv_query           | Complete query string with embedded options
    "! @parameter iv_type            | Object type the query is for
    "! @parameter it_search_term     | List of search terms for the main object type
    "! @parameter it_sub_search_term | List of search terms for the sub object type
    "! @parameter it_search_options  | List of filter options for main/sub query type
    METHODS constructor
      IMPORTING
        iv_query           TYPE string                               OPTIONAL
        iv_type            TYPE zif_sat_ty_object_search=>ty_search_type
        it_search_term     TYPE zif_sat_ty_global=>ty_t_string_range OPTIONAL
        it_sub_search_term TYPE zif_sat_ty_global=>ty_t_string_range OPTIONAL
        it_search_options  TYPE zif_sat_ty_object_search=>ty_t_search_option.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_object_search_query IMPLEMENTATION.
  METHOD constructor.
    mt_search_options = it_search_options.
    mv_query = iv_query.
    mt_search_term = it_search_term.
    mt_sub_search_term = it_sub_search_term.
    mv_type = iv_type.
    mt_search_options = it_search_options.

    mv_max_rows = VALUE #( mt_search_options[ option = zif_sat_c_object_search=>c_general_search_params-max_rows
                           ]-value_range[ 1 ]-low DEFAULT 50 ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~get_option.
    rs_option = VALUE #( mt_search_options[ option = iv_option target = iv_target ] OPTIONAL ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~has_options.
    result = xsdbool( mt_search_options IS NOT INITIAL ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~has_search_terms.
    result = xsdbool( mt_search_term IS NOT INITIAL ).
  ENDMETHOD.

  METHOD zif_sat_object_search_query~set_option.
    ASSIGN mt_search_options[ option = is_option-option ] TO FIELD-SYMBOL(<ls_option>).
    IF sy-subrc = 0.
      <ls_option>-value_range = is_option-value_range.
    ELSE.
      mt_search_options = VALUE #( BASE mt_search_options ( is_option ) ).
    ENDIF.

    IF is_option-option = zif_sat_c_object_search=>c_general_search_params-max_rows.
      mv_max_rows = VALUE #( mt_search_options[ option = zif_sat_c_object_search=>c_general_search_params-max_rows
                             ]-value_range[ 1 ]-low DEFAULT 50 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
