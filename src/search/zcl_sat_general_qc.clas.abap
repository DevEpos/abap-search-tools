"! <p class="shorttext synchronized">Base Converter for Object Search Query</p>
CLASS zcl_sat_general_qc DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_query_converter.

  PROTECTED SECTION.
    "! Converts given value to a number range
    METHODS convert_to_number_range
      IMPORTING
        iv_option       TYPE string
        iv_length       TYPE i
        iv_sign         TYPE ddsign
        iv_value        TYPE string
      RETURNING
        VALUE(rs_value) TYPE zif_sat_ty_object_search=>ty_s_value_range
      RAISING
        zcx_sat_object_search.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_general_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    IF    iv_option = zif_sat_c_object_search=>c_general_search_params-user
       OR iv_option = zif_sat_c_object_search=>c_general_search_params-changed_by.
      IF cv_value = 'ME'.
        cv_value = sy-uname.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD convert_to_number_range.
    DATA lv_val1 TYPE string.
    DATA lv_val1_submatch TYPE submatch_result.

    DATA(lv_range_regex) = |^(\\d\{1,{ iv_length }\})\\.\{3\}(\\d\{1,{ iv_length }\})$|.
    DATA(lv_single_range_regex) = |^(<\|<=\|>\|>=)(\\d\{1,{ iv_length }\})$|.

    IF iv_value CO '0123456789'.
      " Nothing to do
      rs_value-sign   = iv_sign.
      rs_value-option = zif_sat_c_options=>equals.
      rs_value-low    = iv_value.
    ELSE.
      FIND REGEX lv_single_range_regex IN iv_value RESULTS DATA(ls_matches).
      IF ls_matches-length > 0.
        DATA(lv_comp_submatch) = ls_matches-submatches[ 1 ].
        DATA(lv_comp) = iv_value(lv_comp_submatch-length).

        lv_val1_submatch = ls_matches-submatches[ 2 ].
        lv_val1 = iv_value+lv_val1_submatch-offset(lv_val1_submatch-length).

        rs_value-sign   = iv_sign.
        rs_value-option = SWITCH #( lv_comp
                                    WHEN '>'  THEN 'GT'
                                    WHEN '>=' THEN 'GE'
                                    WHEN '<'  THEN 'LT'
                                    WHEN '<=' THEN 'LE' ).
        rs_value-low    = lv_val1.
      ELSE.
        " check allowed range patterns
        FIND REGEX lv_range_regex IN iv_value RESULTS ls_matches.
        IF ls_matches-length > 0.
          lv_val1_submatch = ls_matches-submatches[ 1 ].
          lv_val1 = iv_value(lv_val1_submatch-length).

          DATA(lv_val2_submatch) = ls_matches-submatches[ 2 ].
          DATA(lv_val2) = iv_value+lv_val2_submatch-offset(lv_val2_submatch-length).

          rs_value-sign   = iv_sign.
          rs_value-option = zif_sat_c_options=>between.
          rs_value-low    = lv_val1.
          rs_value-high   = lv_val2.
        ELSE.
          zcx_sat_object_search=>raise_object_search_error(
              iv_text = |Invalid value '{ iv_value }' provided.\nAllowed patterns for filter '{ iv_option }' are: 5...10, <2, <=2, >2, >=5.\n\nMaximum number length: { iv_length }| ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
