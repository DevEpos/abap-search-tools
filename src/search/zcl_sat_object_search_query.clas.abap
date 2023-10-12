"! <p class="shorttext synchronized">Query for Object Search</p>
CLASS zcl_sat_object_search_query DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_query.
    INTERFACES zif_sat_c_object_search.

    ALIASES mt_search_term    FOR zif_sat_object_search_query~mt_search_term.
    ALIASES mv_type           FOR zif_sat_object_search_query~mv_type.
    ALIASES mv_query          FOR zif_sat_object_search_query~mv_query.
    ALIASES mv_max_rows       FOR zif_sat_object_search_query~mv_max_rows.
    ALIASES mt_search_options FOR zif_sat_object_search_query~mt_search_options.

    "! <p class="shorttext synchronized">Creates new Search Query</p>
    "!
    "! @parameter iv_type           | Object type the query is for
    "! @parameter iv_query          | Complete query string with embedded options
    "! @parameter iv_max_rows       | Max rows to be retrieved by query
    "! @parameter it_search_term    | List of search terms for the main object type
    "! @parameter it_search_options | List of filter options for main/sub query type
    "! @parameter is_settings       | Query Settings
    METHODS constructor
      IMPORTING
        iv_type           TYPE zif_sat_ty_object_search=>ty_search_type
        iv_query          TYPE string                                     OPTIONAL
        iv_max_rows       TYPE i                                          OPTIONAL
        it_search_term    TYPE zif_sat_ty_object_search=>ty_t_search_term OPTIONAL
        it_search_options TYPE zif_sat_ty_object_search=>ty_t_search_option.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_object_search_query IMPLEMENTATION.
  METHOD constructor.
    mv_type = iv_type.
    mv_query = iv_query.
    mt_search_term = it_search_term.
    mt_search_options = it_search_options.

    IF iv_max_rows IS SUPPLIED.
      mv_max_rows = iv_max_rows.
    ELSE.
      mv_max_rows = VALUE #( mt_search_options[ option = zif_sat_c_object_search=>c_general_search_params-max_rows
                             ]-value_range[ 1 ]-low DEFAULT 50 ).
    ENDIF.
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
    ASSIGN mt_search_options[ option = is_option-option target = is_option-target ] TO FIELD-SYMBOL(<ls_option>).
    IF sy-subrc = 0.
      <ls_option>-value_range = is_option-value_range.
    ELSE.
      mt_search_options = VALUE #( BASE mt_search_options ( is_option ) ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_sat_object_search_query~set_max_rows.
    mv_max_rows = iv_max_rows.
  ENDMETHOD.

  METHOD zif_sat_object_search_query~to_string.
    DATA lv_sep TYPE string.

    result = '{'.

    IF mt_search_term IS NOT INITIAL.
      result = result && '"terms":['.
      LOOP AT mt_search_term REFERENCE INTO DATA(lr_term).
        result = |{ result }{ lv_sep }\{"{ lr_term->target }":[|.
        CLEAR lv_sep.

        LOOP AT lr_term->values REFERENCE INTO DATA(lr_string_val).
          result = |{ result }{ lv_sep }\{"sign":"{ lr_string_val->sign }"| &&
                     |,"option":"{ lr_string_val->option }","low":"{ lr_string_val->low }"\}|.
          lv_sep = ','.
        ENDLOOP.

        result = result && ']}'.
        lv_sep = ','.
      ENDLOOP.
      result = result && ']'.
      lv_sep = ','.
    ENDIF.

    IF mt_search_options IS NOT INITIAL.
      result = |{ result }{ lv_sep }"searchOptions":[|.
      CLEAR lv_sep.

      LOOP AT mt_search_options REFERENCE INTO DATA(lr_option)
           GROUP BY lr_option->target.

        result = |{ result }{ lv_sep }\{"{ lr_option->target }":[|.
        CLEAR lv_sep.

        LOOP AT GROUP lr_option REFERENCE INTO DATA(lr_option_group).
          result = |{ result }{ lv_sep }\{"{ lr_option_group->option }":[|.
          CLEAR lv_sep.

          LOOP AT lr_option_group->value_range REFERENCE INTO DATA(lr_value).
            result = |{ result }{ lv_sep }\{"sign":"{ lr_value->sign }"| &&
                       |,"option":"{ lr_value->option }","low":"{ lr_value->low }"|.
            IF lr_value->high IS NOT INITIAL.
              result = result && |,"high":"{ lr_value->high }"|.
            ENDIF.
            IF lr_value->option2 IS NOT INITIAL.
              result = |{ result },"option2":"{ lr_value->option2 }","sign2":"{ lr_value->sign2 }"|.
            ENDIF.
            result = |{ result }\}|.
            lv_sep = ','.
          ENDLOOP.

          result = |{ result }]\}|.
          lv_sep = ','.
        ENDLOOP.

        result = |{ result }]\}|.
        lv_sep = ','.
      ENDLOOP.

      result = |{ result }]|.
    ENDIF.

    result = |{ result }\}|.
  ENDMETHOD.
ENDCLASS.
