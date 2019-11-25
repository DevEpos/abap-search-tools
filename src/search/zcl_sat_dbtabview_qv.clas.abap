"! <p class="shorttext synchronized" lang="en">Validator for Database Table/View Search query</p>
CLASS zcl_sat_dbtabview_qv DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qv
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_sat_query_validator~validate_option
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_dbtabview_qv IMPLEMENTATION.

  METHOD zif_sat_query_validator~validate_option.
    DATA: lf_invalid TYPE abap_bool.

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).

    DATA(lv_value) = iv_value.

    IF iv_option = zif_sat_c_object_search=>c_search_option-by_type.
      zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value ).

      CASE lv_value.

        WHEN zif_sat_c_object_search=>c_type_option_value-table OR
             zif_sat_c_object_search=>c_type_option_value-view.

        WHEN OTHERS.
          lf_invalid = abap_true.
      ENDCASE.
    ENDIF.

    IF lf_invalid = abap_true.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>invalid_option_value
          msgv1  = |{ iv_option }|
          msgv2  = |{ lv_value }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
