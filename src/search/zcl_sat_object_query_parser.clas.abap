"! <p class="shorttext synchronized">Parser for object search query</p>
CLASS zcl_sat_object_query_parser DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_query_parser.
    INTERFACES zif_sat_c_object_search.
    INTERFACES zif_sat_ty_object_search.

    ALIASES c_general_search_options FOR zif_sat_c_object_search~c_general_search_params.
    ALIASES c_cds_search_params      FOR zif_sat_c_object_search~c_cds_search_params.
    ALIASES c_class_intf_options     FOR zif_sat_c_object_search~c_class_intf_search_option.
    ALIASES ty_t_search_option       FOR zif_sat_ty_object_search~ty_t_search_option.
    ALIASES ty_s_search_option       FOR zif_sat_ty_object_search~ty_s_search_option.

    METHODS constructor
      IMPORTING
        io_configuration TYPE REF TO zif_sat_object_search_config
        io_validator     TYPE REF TO zif_sat_query_validator
        io_converter     TYPE REF TO zif_sat_query_converter.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_allowed_option_by_type,
             type   TYPE char1,
             option TYPE string,
           END OF ty_allowed_option_by_type.
    TYPES ty_lt_allowed_options TYPE RANGE OF string.

    CONSTANTS c_option_separator TYPE string VALUE ':' ##NO_TEXT.
    CONSTANTS c_value_separator TYPE string VALUE ',' ##NO_TEXT.
    CONSTANTS c_negation_operator TYPE string VALUE '!' ##NO_TEXT.
    CONSTANTS c_key_value_pair_separator TYPE string VALUE '=' ##NO_TEXT.

    "! <p class="shorttext synchronized">Retrieves the search terms from the given string</p>
    METHODS get_search_terms
      IMPORTING
        iv_search_string      TYPE string
      RETURNING
        VALUE(rt_search_term) TYPE zif_sat_ty_object_search=>ty_t_search_term.

    "! <p class="shorttext synchronized">Checks the given option and its values</p>
    METHODS parse_option
      IMPORTING
        is_option  TYPE ty_s_search_option
      CHANGING
        ct_options TYPE ty_t_search_option
      RAISING
        zcx_sat_object_search.

    "! <p class="shorttext synchronized">Extract search option from token</p>
    "! Splits token at character ':' and collects the option and its value
    "! if it exists in the list of allowed search options
    "!
    METHODS extract_option
      IMPORTING
        iv_token   TYPE string
      CHANGING
        ct_options TYPE ty_t_search_option
      RAISING
        zcx_sat_object_search.

    "! <p class="shorttext synchronized">Adds value for option</p>
    "!
    METHODS add_option_value
      IMPORTING
        is_option  TYPE zif_sat_ty_object_search=>ty_query_filter
        iv_target  TYPE string OPTIONAL
        iv_value   TYPE string
      CHANGING
        ct_options TYPE ty_t_search_option
      RAISING
        zcx_sat_object_search.

  PRIVATE SECTION.
    DATA mo_configuration TYPE REF TO zif_sat_object_search_config.
    DATA mo_validator TYPE REF TO zif_sat_query_validator.
    DATA mo_converter TYPE REF TO zif_sat_query_converter.

    METHODS convert_term_to_selopt
      IMPORTING
        iv_term           TYPE string
        if_case_sensitive TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)     TYPE LINE OF zif_sat_ty_global=>ty_t_string_range.

    METHODS convert_to_selopt_terms
      CHANGING
        ct_search_terms TYPE zif_sat_ty_object_search=>ty_t_search_term.
ENDCLASS.


CLASS zcl_sat_object_query_parser IMPLEMENTATION.
  METHOD constructor.
    mo_configuration = io_configuration.
    mo_validator = io_validator.
    mo_converter = io_converter.
  ENDMETHOD.

  METHOD zif_sat_object_query_parser~parse_query.
    DATA lt_query_tokens TYPE TABLE OF string.
    DATA lv_search_terms TYPE string.
    DATA lt_options TYPE ty_t_search_option.

    IF iv_search_query IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING textid = zcx_sat_object_search=>no_query_string.
    ENDIF.

    SPLIT iv_search_query AT space INTO TABLE lt_query_tokens.

    LOOP AT lt_query_tokens ASSIGNING FIELD-SYMBOL(<lv_token>).
      IF <lv_token> CS c_option_separator.
        extract_option( EXPORTING iv_token   = <lv_token>
                        CHANGING  ct_options = lt_options ).
      ELSE.
        IF lv_search_terms IS INITIAL.
          lv_search_terms = <lv_token>.
        ELSE.
          lv_search_terms = |{ lv_search_terms } { <lv_token> }|.
        ENDIF.
      ENDIF.
    ENDLOOP.

    mo_validator->check_option_integrity( CHANGING ct_options = lt_options ).

    ro_query = NEW zcl_sat_object_search_query( iv_query          = iv_search_query
                                                iv_type           = mo_configuration->get_type( )
                                                it_search_term    = get_search_terms( lv_search_terms )
                                                it_search_options = lt_options ).
  ENDMETHOD.

  METHOD zif_sat_object_query_parser~create_query.
    DATA lt_options LIKE it_options.

    LOOP AT it_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      parse_option( EXPORTING is_option  = <ls_option>
                    CHANGING  ct_options = lt_options ).
    ENDLOOP.

    mo_validator->check_option_integrity( CHANGING ct_options = lt_options ).

    DATA(lt_search_terms) = it_search_terms.
    convert_to_selopt_terms( CHANGING ct_search_terms = lt_search_terms ).

    IF     lt_search_terms IS INITIAL
       AND lt_options      IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING textid = zcx_sat_object_search=>empty_query.
    ENDIF.

    ro_query = NEW zcl_sat_object_search_query( iv_type           = mo_configuration->get_type( )
                                                it_search_term    = lt_search_terms
                                                it_search_options = lt_options ).
  ENDMETHOD.

  METHOD parse_option.
    DATA(lt_value_range) = is_option-value_range.

    IF NOT mo_configuration->has_option( iv_option = is_option-option iv_target = is_option-target ).
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING textid = zcx_sat_object_search=>invalid_query_option
                  msgv1  = |{ is_option-option }|.
    ENDIF.
    DATA(ls_option_config) = mo_configuration->get_option_config( iv_option = is_option-option
                                                                  iv_target = is_option-target ).

    LOOP AT lt_value_range ASSIGNING FIELD-SYMBOL(<ls_value_range>).
      add_option_value( EXPORTING is_option  = ls_option_config
                                  iv_target  = is_option-target
                                  iv_value   = <ls_value_range>-low
                        CHANGING  ct_options = ct_options ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_search_terms.
    DATA lt_word TYPE TABLE OF string.
    DATA ls_search_term TYPE zif_sat_ty_object_search=>ty_s_search_term.

    CHECK iv_search_string IS NOT INITIAL.

    ls_search_term-target = zif_sat_c_object_search=>c_search_fields-object_name_input_key.

    DATA(lv_search_string) = iv_search_string.
    CONDENSE lv_search_string.
    SPLIT lv_search_string AT space INTO TABLE lt_word.

    LOOP AT lt_word INTO DATA(lv_search_term).
      ls_search_term-values = VALUE #( BASE ls_search_term-values
                                       ( convert_term_to_selopt( iv_term = lv_search_term ) ) ).
    ENDLOOP.

    rt_search_term = VALUE #( ( ls_search_term ) ).
  ENDMETHOD.

  METHOD add_option_value.
    ASSIGN ct_options[ option = is_option-name target = iv_target ] TO FIELD-SYMBOL(<ls_option>).
    IF sy-subrc <> 0.
      INSERT VALUE #( option = is_option-name target = iv_target )
      INTO TABLE ct_options ASSIGNING <ls_option>.
    ENDIF.

    DATA(lv_value) = iv_value.
    DATA(lv_value2) = ``.
    DATA(lv_sign) = zif_sat_c_options=>including.
    DATA(lv_sign2) = zif_sat_c_options=>including.

    IF is_option-no_negation = abap_false.
      zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value
                                                             cv_sign  = lv_sign ).
    ENDIF.

    " Consider key-value options in a special way
    IF is_option-key_value = abap_true.
      IF lv_value CS c_key_value_pair_separator.
        SPLIT lv_value AT c_key_value_pair_separator INTO lv_value lv_value2.
        TRANSLATE lv_value TO UPPER CASE.
        " Ignore second value if first value is already excluded
        IF lv_sign = zif_sat_c_options=>excluding.
          CLEAR: lv_value2,
                 lv_sign2.
        ELSE.
          IF is_option-no_uppercase = abap_false.
            TRANSLATE lv_value2 TO UPPER CASE.
          ENDIF.
          IF is_option-no_negation = abap_false.
            zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value2
                                                                   cv_sign  = lv_sign2 ).
          ENDIF.
        ENDIF.
      ELSE.
        TRANSLATE lv_value TO UPPER CASE.
      ENDIF.
    ELSE.
      IF is_option-no_uppercase = abap_false.
        TRANSLATE lv_value TO UPPER CASE.
      ENDIF.
    ENDIF.

    " Translate to Open SQL Wildcard character
    lv_value = replace( val = lv_value sub = '?' occ = 0  with = '+' ).
    lv_value2 = replace( val = lv_value2 sub = '?' occ = 0  with = '+' ).

    mo_validator->validate_option( iv_option         = is_option-name
                                   is_content_assist = is_option-content_assist
                                   iv_target         = iv_target
                                   iv_value          = lv_value
                                   iv_value2         = lv_value2 ).

    IF is_option-data_type = zif_sat_c_object_search=>c_filter_data_type-boolean.
      lv_value = to_upper( lv_value ).
      lv_value = xsdbool( lv_value = 'TRUE' OR lv_value = abap_true ).
    ELSE.
      mo_converter->convert_value( EXPORTING iv_option = is_option-name
                                             iv_target = iv_target
                                   CHANGING  cv_value  = lv_value
                                             cv_value2 = lv_value2 ).
    ENDIF.

    " Crop input if necessary
    IF     lv_value                 NA '*+'
       AND is_option-allowed_length  > 0
       AND strlen( lv_value )        > is_option-allowed_length.
      lv_value = lv_value(is_option-allowed_length).
    ENDIF.

    <ls_option>-value_range = VALUE #(
        BASE <ls_option>-value_range
        ( sign    = lv_sign
          sign2   = lv_sign2
          option  = COND #( WHEN lv_value CA '*+'
                            THEN zif_sat_c_options=>contains_pattern
                            ELSE zif_sat_c_options=>equals )
          option2 = COND #( WHEN lv_value2 IS INITIAL THEN space
                            WHEN lv_value2 CA '*+'    THEN zif_sat_c_options=>contains_pattern
                            ELSE                           zif_sat_c_options=>equals )
          low     = lv_value
          high    = lv_value2 ) ).
  ENDMETHOD.

  METHOD extract_option.
    DATA lv_option TYPE string.
    DATA lv_value_list TYPE string.
    DATA lt_values TYPE string_table.

    SPLIT iv_token AT c_option_separator INTO lv_option lv_value_list.
    TRANSLATE lv_option TO LOWER CASE.
    mo_configuration->map_option( CHANGING cv_option = lv_option ).

    IF NOT mo_configuration->has_option( lv_option ).
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING textid = zcx_sat_object_search=>invalid_query_option
                  msgv1  = |{ lv_option }|.
    ENDIF.

    DATA(ls_option_info) = mo_configuration->get_option_config( lv_option ).

    " Get all included values for this option
    IF lv_value_list CS c_value_separator.
      IF ls_option_info-single = abap_true.
        RAISE EXCEPTION TYPE zcx_sat_object_search
          EXPORTING textid = zcx_sat_object_search=>no_intervals_for_option
                    msgv1  = |{ SWITCH #(
                                  lv_option
                                  WHEN c_general_search_options-max_rows
                                  THEN |{ lv_option }({ 'Max Rows'(001) })|
                                  ELSE lv_option )
                                }|.
      ENDIF.

      SPLIT lv_value_list AT c_value_separator INTO TABLE lt_values.
      LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<lv_value>).
        add_option_value( EXPORTING is_option  = ls_option_info
                                    iv_target  = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                    iv_value   = <lv_value>
                          CHANGING  ct_options = ct_options ).
      ENDLOOP.
    ELSE.
      " Only a single value is included in the query
      add_option_value( EXPORTING is_option  = ls_option_info
                                  iv_target  = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                  iv_value   = lv_value_list
                        CHANGING  ct_options = ct_options ).
    ENDIF.

    " delete duplicate entries
    LOOP AT ct_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      SORT <ls_option>-value_range BY sign
                                      option
                                      low
                                      high.
      DELETE ADJACENT DUPLICATES FROM <ls_option>-value_range COMPARING sign option low high.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_term_to_selopt.
    DATA(lv_term) = iv_term.
    lv_term = replace( val = lv_term sub = '?' occ = 0  with = '+' ).
    DATA(lv_length) = strlen( lv_term ).
    DATA(lv_last_char_offset) = lv_length - 1.

    DATA(lv_option) = zif_sat_c_options=>contains_pattern.
    DATA(lv_sign) = zif_sat_c_options=>including.

    IF lv_term+lv_last_char_offset(1) = '<'.
      lv_term = lv_term(lv_last_char_offset).
      IF lv_term NA '+*'.
        lv_option = zif_sat_c_options=>equals.
      ENDIF.
    ELSEIF lv_term+lv_last_char_offset(1) <> '*'.
      lv_term = |{ lv_term }*|.
    ENDIF.

    " Check if string should be negated
    IF lv_term(1) = c_negation_operator.
      lv_sign = zif_sat_c_options=>excluding.
      lv_term = lv_term+1.
    ENDIF.

    result = VALUE #( sign   = lv_sign
                      option = lv_option
                      low    = COND #( WHEN if_case_sensitive = abap_true THEN lv_term ELSE to_upper( lv_term ) ) ).
  ENDMETHOD.

  METHOD convert_to_selopt_terms.
    DATA(lt_fields) = mo_configuration->get_search_config( )-inputs.

    LOOP AT ct_search_terms REFERENCE INTO DATA(lr_search_term).
      " find field configuration to determine if 'to_upper' should be performed
      DATA(lf_field_case_sensitive) = VALUE #( lt_fields[ name = lr_search_term->target ]-case_sensitive OPTIONAL ).

      LOOP AT lr_search_term->values REFERENCE INTO DATA(lr_term_value).
        IF lr_term_value->sign IS INITIAL OR lr_term_value->option IS INITIAL.
          lr_term_value->* = convert_term_to_selopt( iv_term = lr_term_value->low if_case_sensitive = lf_field_case_sensitive ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

