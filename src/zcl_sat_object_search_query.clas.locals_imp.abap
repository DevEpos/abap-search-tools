*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_query_option_validator IMPLEMENTATION.

  METHOD create_validator.
    CASE iv_type.

      WHEN zif_sat_c_object_browser_mode=>cds_view.
        rr_validator = NEW lcl_qov_cds( ).

      WHEN zif_sat_c_object_browser_mode=>database_table_view.
        rr_validator = NEW lcl_qov_database_tab_view( ).

      WHEN zif_sat_c_object_browser_mode=>query.
        rr_validator = NEW lcl_qov_query( ).

      WHEN zif_sat_c_object_browser_mode=>package.
        rr_validator = NEW lcl_qov_package( ).

      WHEN OTHERS.
        rr_validator = NEW lcl_qov_default( ).
    ENDCASE.

  ENDMETHOD.

  METHOD validate.
    IF iv_value IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>option_incomplete
          msgv1  = |{ iv_option }|.
    ENDIF.

    CASE iv_option.

      WHEN zif_sat_c_object_search=>c_search_option-max_rows.
        IF iv_value CN '0123456789'.
          RAISE EXCEPTION TYPE zcx_sat_object_search
            EXPORTING
              textid = zcx_sat_object_search=>option_val_not_numeric
              msgv1  = |{ iv_option }|.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_qov_cds IMPLEMENTATION.

  METHOD validate.
    DATA: lf_invalid TYPE abap_bool.

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).

*.. Remove exclusion characters before the actual validation
    DATA(lv_value) = iv_value.

    IF iv_option = zif_sat_c_object_search=>c_search_option-by_type.
      lcl_exclusion_helper=>remove_exclusion_string( CHANGING cv_value = lv_value ).
      CASE lv_value.

        WHEN zif_sat_c_object_search=>c_type_option_value-function OR
             zif_sat_c_object_search=>c_type_option_value-hierarchy OR
             zif_sat_c_object_search=>c_type_option_value-abstract_entity OR
             zif_sat_c_object_search=>c_type_option_value-custom_entity OR
             zif_sat_c_object_search=>c_type_option_value-view OR
             zif_sat_c_object_search=>c_type_option_value-extend.

        WHEN OTHERS.
          lf_invalid = abap_true.
      ENDCASE.
    ELSEIF iv_option = zif_sat_c_object_search=>c_search_option-by_params.
      IF lv_value <> 'TRUE' AND lv_value <> 'FALSE'.
        lf_invalid = abap_true.
      ENDIF.
    ENDIF.

    IF lf_invalid = abap_true.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>invalid_option_value
          msgv1  = |{ iv_option }|
          msgv2  = |{ lv_value }|.
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_qov_database_tab_view IMPLEMENTATION.

  METHOD validate.
    DATA: lf_invalid TYPE abap_bool.

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).

    DATA(lv_value) = iv_value.

    IF iv_option = zif_sat_c_object_search=>c_search_option-by_type.
      lcl_exclusion_helper=>remove_exclusion_string( CHANGING cv_value = lv_value ).
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

CLASS lcl_qov_query IMPLEMENTATION.

  METHOD validate.
    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_qov_package IMPLEMENTATION.

  METHOD validate.
    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_exclusion_helper IMPLEMENTATION.

  METHOD remove_exclusion_string.
    IF cv_value CP '!*'.
      cv_value = cv_value+1.
      cv_sign = zif_sat_c_options=>excluding.
    ENDIF.

    IF cv_value CP '<>*'.
      cv_value = cv_value+2.
      cv_sign = zif_sat_c_options=>excluding.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_qov_default IMPLEMENTATION.

  METHOD validate.
    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).
  ENDMETHOD.

ENDCLASS.
