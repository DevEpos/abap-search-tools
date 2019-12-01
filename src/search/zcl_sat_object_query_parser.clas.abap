"! <p class="shorttext synchronized" lang="en">Parser for object search query</p>
CLASS zcl_sat_object_query_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_object_query_parser.
    INTERFACES zif_sat_c_object_search.
    INTERFACES zif_sat_ty_object_search.
    ALIASES:
      c_search_option FOR zif_sat_c_object_search~c_search_option,
      c_class_intf_options FOR zif_sat_c_object_search~c_class_intf_search_option,
      ty_t_search_option FOR zif_sat_ty_object_search~ty_t_search_option,
      ty_s_search_option FOR zif_sat_ty_object_search~ty_s_search_option.
    METHODS constructor
      IMPORTING
        io_configuration TYPE REF TO zif_sat_object_search_config
        io_validator     TYPE REF TO zif_sat_query_validator.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_allowed_option_by_type,
             type   TYPE char1,
             option TYPE string,
           END OF ty_allowed_option_by_type.
    TYPES: BEGIN OF ty_option_setting,
             option         TYPE string,
             allowed_length TYPE i,
             single         TYPE abap_bool,
             key_value      TYPE abap_bool,
             no_negation    TYPE abap_bool,
           END OF ty_option_setting.
    TYPES:
      ty_lt_allowed_options TYPE RANGE OF string.
    CONSTANTS c_option_separator TYPE string VALUE ':' ##NO_TEXT.
    CONSTANTS c_value_separator TYPE string VALUE ',' ##NO_TEXT.
    CONSTANTS c_negation_operator TYPE string VALUE '!' ##no_text.
    CONSTANTS c_key_value_pair_separator TYPE string VALUE '=' ##no_text.


    "! <p class="shorttext synchronized" lang="en">Retrieves the search terms from the given string</p>
    METHODS get_search_terms
      IMPORTING
        iv_search_string      TYPE string
      RETURNING
        VALUE(rt_search_term) TYPE zif_sat_ty_global=>ty_t_string_range.
    "! <p class="shorttext synchronized" lang="en">Checks the given option and its values</p>
    METHODS parse_option
      IMPORTING
        is_option  TYPE ty_s_search_option
      CHANGING
        ct_options TYPE ty_t_search_option
      RAISING
        zcx_sat_object_search.
    "! <p class="shorttext synchronized" lang="en">Enhance certain query options</p>
    METHODS enhance_options
      CHANGING
        ct_options TYPE ty_t_search_option
      RAISING
        zcx_sat_object_search.
    "! <p class="shorttext synchronized" lang="en">Converts option value for correct selection</p>
    "!
    METHODS convert_option_value
      IMPORTING
        iv_option TYPE string
      CHANGING
        cv_value  TYPE string.
    "! <p class="shorttext synchronized" lang="en">Extract search option from token</p>
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
    "! <p class="shorttext synchronized" lang="en">Adds value for option</p>
    "!
    METHODS add_option_value
      IMPORTING
        is_option  TYPE ty_option_setting
        iv_value   TYPE string
      CHANGING
        ct_options TYPE ty_t_search_option.

  PRIVATE SECTION.
    DATA mo_configuration TYPE REF TO zif_sat_object_search_config.
    DATA mo_validator TYPE REF TO zif_sat_query_validator.
ENDCLASS.



CLASS zcl_sat_object_query_parser IMPLEMENTATION.

  METHOD constructor.
    mo_configuration = io_configuration.
    mo_validator = io_validator.
  ENDMETHOD.

  METHOD zif_sat_object_query_parser~parse_query.
    DATA: lt_query_tokens       TYPE TABLE OF string,
          lv_search_terms       TYPE string,
          lt_options            TYPE ty_t_search_option,
          lv_option             TYPE string,
          lv_value              TYPE string,
          lf_query_string_found TYPE abap_bool.

    FIELD-SYMBOLS: <ls_option_value> TYPE ty_s_search_option.

    IF iv_search_query IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>no_query_string.
    ENDIF.

*.. Get all possible options for the given search type
    DATA(lt_allowed_options) = mo_configuration->get_allowed_options( ).

    SPLIT iv_search_query AT space INTO TABLE lt_query_tokens.

    LOOP AT lt_query_tokens ASSIGNING FIELD-SYMBOL(<lv_token>).
      CLEAR: lv_option, lv_value.

      IF <lv_token> CS c_option_separator.
        extract_option(
          EXPORTING iv_token   = <lv_token>
          CHANGING  ct_options = lt_options
        ).
      ELSE.
        IF lv_search_terms IS INITIAL.
          lv_search_terms = <lv_token>.
        ELSE.
          lv_search_terms = |{ lv_search_terms } { <lv_token> }|.
        ENDIF.
      ENDIF.
    ENDLOOP.


    mo_validator->check_option_integrity( CHANGING ct_options = lt_options ).

    enhance_options( CHANGING ct_options = lt_options ).

    ro_query = NEW zcl_sat_object_search_query(
      iv_query                = lv_search_terms
      iv_type                 = mo_configuration->get_type( )
      it_search_term          = get_search_terms( lv_search_terms )
      it_search_options       = lt_options
    ).
  ENDMETHOD.

  METHOD zif_sat_object_query_parser~create_query.
    DATA: lt_options LIKE it_options.

    LOOP AT it_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      parse_option(
        EXPORTING is_option      = <ls_option>
        CHANGING  ct_options     = lt_options ).
    ENDLOOP.

    mo_validator->check_option_integrity( CHANGING ct_options = lt_options ).

    enhance_options( CHANGING ct_options = lt_options ).

    DATA(lt_search_terms) = get_search_terms( iv_search_terms ).

    IF lt_search_terms IS INITIAL AND
       lt_options IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>empty_query.
    ENDIF.

    ro_query = NEW zcl_sat_object_search_query(
      iv_type                 = mo_configuration->get_type( )
      it_search_term          = lt_search_terms
      it_search_options       = lt_options
    ).
  ENDMETHOD.

  METHOD parse_option.
    DATA(lt_value_range) = is_option-value_range.

    DATA(ls_option_config) = mo_configuration->get_option_config( is_option-option ).

    LOOP AT lt_value_range ASSIGNING FIELD-SYMBOL(<ls_value_range>).
      DATA(lv_value) = <ls_value_range>-low.
      convert_option_value( EXPORTING iv_option = is_option-option CHANGING cv_value = lv_value ).
      mo_validator->validate_option( iv_option = is_option-option iv_value = lv_value ).
      add_option_value(
        EXPORTING is_option  = ls_option_config
                  iv_value   = lv_value
        CHANGING  ct_options = ct_options
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_option_value.
    IF sy-saprl >= 751. " upper() function in CDS view
      TRANSLATE cv_value TO UPPER CASE.
    ELSE.
      CASE iv_option.

        WHEN c_search_option-by_description OR
             c_search_option-by_anno.

        WHEN OTHERS.
          TRANSLATE cv_value TO UPPER CASE.
      ENDCASE.
    ENDIF.

    IF iv_option = c_search_option-by_owner.
      IF  cv_value = 'ME'.
        cv_value = sy-uname.
      ELSEIF cv_value CP '!ME'.
        cv_value = c_negation_operator && sy-uname.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_search_terms.
    DATA: lt_search_terms TYPE TABLE OF string.

    CHECK iv_search_string IS NOT INITIAL.

    DATA(lv_search_string) = iv_search_string.
    CONDENSE lv_search_string.
    SPLIT lv_search_string AT space INTO TABLE lt_search_terms.

    LOOP AT lt_search_terms INTO DATA(lv_search_term).
      lv_search_term = replace( val = lv_search_term sub = '?' occ = 0  with = '+' ).
      DATA(lv_length) = strlen( lv_search_term ).
      DATA(lv_last_char_offset) = lv_length - 1.

      DATA(lv_option) = zif_sat_c_options=>contains_pattern.
      DATA(lv_sign) = zif_sat_c_options=>including.

      IF lv_search_term+lv_last_char_offset(1) = '<'.
        lv_search_term = lv_search_term(lv_last_char_offset).
        IF lv_search_term NA '+*'.
          lv_option = zif_sat_c_options=>equals.
        ENDIF.
      ELSEIF lv_search_term+lv_last_char_offset(1) <> '*'.
        lv_search_term = |{ lv_search_term }*|.
      ENDIF.

*.... Check if string should be negated
      IF lv_search_term(1) = c_negation_operator.
        lv_sign = zif_sat_c_options=>excluding.
        lv_search_term = lv_search_term+1.
      ENDIF.

      rt_search_term = VALUE #( BASE rt_search_term ( sign = lv_sign option = lv_option low = to_upper( lv_search_term ) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_option_value.
    ASSIGN ct_options[ option = is_option-option ] TO FIELD-SYMBOL(<ls_option>).
    IF sy-subrc <> 0.
      INSERT VALUE #(
        option = is_option-option
      ) INTO TABLE ct_options ASSIGNING <ls_option>.
    ENDIF.

    DATA(lv_value) = iv_value.
    DATA(lv_value2) = ``.
    DATA(lv_sign) = zif_sat_c_options=>including.
    DATA(lv_sign2) = zif_sat_c_options=>including.

    IF is_option-no_negation = abap_false.
      zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value
                                                              cv_sign  = lv_sign  ).
    ENDIF.

*.. Consider key-value options in a special way
    IF is_option-key_value = abap_true.
      IF lv_value CS c_key_value_pair_separator.
        SPLIT lv_value AT c_key_value_pair_separator INTO lv_value lv_value2.
        TRANSLATE lv_value TO UPPER CASE.
*...... Ignore second value if first value is already excluded
        IF lv_sign = zif_sat_c_options=>excluding.
          CLEAR: lv_value2,
                 lv_sign2.
        ELSE.
          IF sy-saprl >= 751. " upper() function available!
            TRANSLATE lv_value2 TO UPPER CASE.
          ENDIF.

          IF is_option-no_negation = abap_false.
            zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value2
                                                                    cv_sign  = lv_sign2   ).
          ENDIF.
        ENDIF.
      ELSE.
        TRANSLATE lv_value TO UPPER CASE.
      ENDIF.
    ENDIF.

*.. Special case for annotation value. As it is possible to annotate via
*... Array/Object notation it is required to prefix the dots with a wildcard as
*... annotations are stored with a <annokey1>$[0-9]$.<annokey2> like syntax
    IF is_option-option = c_search_option-by_anno.
      lv_value = replace( val = lv_value occ = 0 sub = '.' with = '*.' ).
      DATA(lv_value_length) = strlen( lv_value ) - 1.
**      IF lv_value IS NOT INITIAL AND lv_value+lv_value_length(1) <> '*'.
**        lv_value = lv_value && '*'.
**      ENDIF.
    ENDIF.

*.. Translate to Open SQL Wildcard character
    lv_value = replace( val = lv_value sub = '?' occ = 0  with = '+' ).
    lv_value2 = replace( val = lv_value2 sub = '?' occ = 0  with = '+' ).

*.. Crop input if necessary
    IF lv_value NA '*+' AND
       is_option-allowed_length > 0 AND
       strlen( lv_value ) > is_option-allowed_length.
      lv_value = lv_value(is_option-allowed_length).
    ENDIF.


    <ls_option>-value_range = VALUE #(
        BASE <ls_option>-value_range
        ( sign    = lv_sign
          sign2   = lv_sign2
          option  = COND #( WHEN lv_value CA '*+' THEN zif_sat_c_options=>contains_pattern
                            ELSE                       zif_sat_c_options=>equals )
          option2 = COND #( WHEN lv_value2 IS INITIAL THEN space
                            WHEN lv_value2 CA '*+'    THEN zif_sat_c_options=>contains_pattern
                            ELSE                           zif_sat_c_options=>equals )
          low     = lv_value
          high    = lv_value2 )
    ).
  ENDMETHOD.

  METHOD extract_option.

    DATA: lv_option     TYPE string,
          lv_value_list TYPE string,
          lt_values     TYPE string_table.

    SPLIT iv_token AT c_option_separator INTO lv_option lv_value_list.
    TRANSLATE lv_option TO UPPER CASE.
    mo_configuration->map_option( changing cv_option = lv_option ).

    IF NOT mo_configuration->has_option( lv_option ).
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>invalid_query_option
          msgv1  = |{ lv_option }|.
    ENDIF.

    DATA(ls_option_info) = mo_configuration->get_option_config( lv_option ).

*.. Get all included values for this option
    IF lv_value_list CS c_value_separator.
      IF ls_option_info-single = abap_true.
        RAISE EXCEPTION TYPE zcx_sat_object_search
          EXPORTING
            textid = zcx_sat_object_search=>no_intervals_for_option
            msgv1  = |{ SWITCH #(
                          lv_option
                          WHEN c_search_option-max_rows THEN
                            |{ lv_option }({ 'Max Rows'(001) })|
                          ELSE lv_option
                        ) }|.
      ENDIF.

      SPLIT lv_value_list AT c_value_separator INTO TABLE lt_values.
      LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<lv_value>).
        convert_option_value( EXPORTING iv_option = lv_option CHANGING cv_value = <lv_value> ).
        mo_validator->validate_option( iv_option = lv_option iv_value = <lv_value> ).
        add_option_value(
          EXPORTING is_option  = ls_option_info
                    iv_value   = <lv_value>
          CHANGING  ct_options = ct_options
        ).
      ENDLOOP.
    ELSE.
*.... Only a single value is included in the query
      convert_option_value( EXPORTING iv_option = lv_option CHANGING cv_value = lv_value_list ).
      mo_validator->validate_option( iv_option = lv_option iv_value = lv_value_list ).
      add_option_value(
        EXPORTING is_option  = ls_option_info
                  iv_value   = lv_value_list
        CHANGING  ct_options = ct_options
      ).
    ENDIF.

*... delete duplicate entries
    LOOP AT ct_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      SORT <ls_option>-value_range BY sign option low high.
      DELETE ADJACENT DUPLICATES FROM <ls_option>-value_range COMPARING sign option low high.
    ENDLOOP.

  ENDMETHOD.


  METHOD enhance_options.
    DATA: lt_package_range TYPE zif_sat_ty_object_search=>ty_t_value_range.

    LOOP AT ct_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      IF <ls_option>-option = c_search_option-by_package.
        lt_package_range = VALUE #( FOR range IN <ls_option>-value_range WHERE ( option = zif_sat_c_options=>contains_pattern ) ( range ) ).

        LOOP AT <ls_option>-value_range ASSIGNING FIELD-SYMBOL(<ls_value_range>) WHERE option <> zif_sat_c_options=>contains_pattern.
          cl_pak_package_queries=>get_all_subpackages(
            EXPORTING
              im_package             = CONV #( <ls_value_range>-low )
              im_also_local_packages = abap_true
            IMPORTING
              et_subpackages         = DATA(lt_subpackages)
            EXCEPTIONS
              OTHERS                 = 1
          ).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_sat_object_search
              EXPORTING
                textid = zcx_sat_object_search=>invalid_package
                msgv1  = |{ <ls_value_range>-low }|.
          ENDIF.
          lt_package_range = VALUE #( BASE lt_package_range
            ( sign   = <ls_value_range>-sign
              option = zif_sat_c_options=>equals
              low    = <ls_value_range>-low )
            ( LINES OF VALUE #( FOR subpackage IN lt_subpackages ( sign   = <ls_value_range>-sign
                                                                   option = zif_sat_c_options=>equals
                                                                   low    = subpackage-package ) ) )
          ).
        ENDLOOP.

        <ls_option>-value_range = lt_package_range.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
