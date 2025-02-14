CLASS zcl_sat_data_converter DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS perform_alpha_conversion_input
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname
        iv_value     TYPE clike
      EXPORTING
        ev_output    TYPE clike.

    CLASS-METHODS convert_values_to_disp_format
      IMPORTING
        iv_rollname TYPE rollname   OPTIONAL
        iv_type     TYPE inttype    OPTIONAL
        iv_length   TYPE i          DEFAULT 0
        iv_decimals TYPE i          DEFAULT 0
      CHANGING
        cv_value1   TYPE zsat_value OPTIONAL
        cv_value2   TYPE zsat_value OPTIONAL.

    CLASS-METHODS convert_selopt_to_disp_format
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname
      CHANGING
        cv_value1    TYPE zsat_value OPTIONAL
        cv_value2    TYPE zsat_value OPTIONAL.

    CLASS-METHODS convert_values_to_int_format
      IMPORTING
        iv_rollname            TYPE rollname   OPTIONAL
        iv_type                TYPE inttype    OPTIONAL
        iv_length              TYPE i          DEFAULT 0
        iv_decimals            TYPE i          DEFAULT 0
        if_print_error_message TYPE abap_bool  DEFAULT abap_true
      CHANGING
        cv_value1              TYPE zsat_value OPTIONAL
        cv_value2              TYPE zsat_value OPTIONAL.

    CLASS-METHODS convert_selopt_to_int_format
      IMPORTING
        iv_tabname             TYPE tabname
        iv_fieldname           TYPE fieldname
        if_print_error_message TYPE abap_bool  DEFAULT abap_true
      CHANGING
        cv_value1              TYPE zsat_value OPTIONAL
        cv_value2              TYPE zsat_value OPTIONAL.

    CLASS-METHODS convert_dates_to_int_format
      IMPORTING
        iv_tabname             TYPE tabname
        iv_fieldname           TYPE fieldname
        if_print_error_message TYPE abap_bool DEFAULT abap_true
      CHANGING
        cv_value1              TYPE zsat_value
        cv_value2              TYPE zsat_value.

    CLASS-METHODS convert_dates_to_out_format
      IMPORTING
        iv_tabname   TYPE tabname
        iv_fieldname TYPE fieldname
      CHANGING
        cv_value1    TYPE zsat_value OPTIONAL
        cv_value2    TYPE zsat_value OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_data_converter IMPLEMENTATION.
  METHOD convert_dates_to_int_format.
    " TODO: parameter IV_TABNAME is never used (ABAP cleaner)
    " TODO: parameter IV_FIELDNAME is never used (ABAP cleaner)
    " TODO: parameter IF_PRINT_ERROR_MESSAGE is never used (ABAP cleaner)

*&---------------------------------------------------------------------*
*& Description: Conversion of dates into internal format
*& Special case: check if entered value equals to sy-datum
*&---------------------------------------------------------------------*
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lf_convert_value1) = xsdbool( cv_value1 <> 'SY-DATUM' ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lf_convert_value2) = xsdbool( cv_value2 <> 'SY-DATUM' ).
  ENDMETHOD.

  METHOD convert_dates_to_out_format.
*&---------------------------------------------------------------------*
*& Description: Conversion of dates into internal format
*& Special case: check if entered value equals to sy-datum
*&---------------------------------------------------------------------*
    CHECK cv_value1 <> space OR cv_value2 <> space.

    DATA(lr_converter) = NEW cl_fobu_input_util( tabname   = iv_tabname
                                                 fieldname = iv_fieldname ).

    IF cv_value1 <> space.
      IF cv_value1 = 'SY-DATUM'.
        cv_value1 = |{ sy-datum DATE = USER }|.
      ELSE.
        lr_converter->output_convert( EXPORTING field_value_int = cv_value1
                                      IMPORTING field_value_ext = cv_value1 ).
      ENDIF.
    ENDIF.

    IF cv_value2 <> space.
      IF cv_value2 = 'SY-DATUM'.
        cv_value2 = |{ sy-datum DATE = USER }|.
      ELSE.
        lr_converter->output_convert( EXPORTING field_value_int = cv_value2
                                      IMPORTING field_value_ext = cv_value2 ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD convert_selopt_to_disp_format.
*&---------------------------------------------------------------------*
*& Description: Converts input values to display format
*&---------------------------------------------------------------------*
    CHECK cv_value1 <> space OR cv_value2 <> space.

    CHECK: iv_tabname IS NOT INITIAL,
           iv_fieldname IS NOT INITIAL.

    DATA(lr_converter) = NEW cl_fobu_input_util( tabname   = iv_tabname
                                                 fieldname = iv_fieldname ).

    IF cv_value1 <> space.
      lr_converter->output_convert( EXPORTING field_value_int = cv_value1
                                    IMPORTING field_value_ext = cv_value1 ).
    ENDIF.

    IF cv_value2 <> space.
      lr_converter->output_convert( EXPORTING field_value_int = cv_value2
                                    IMPORTING field_value_ext = cv_value2 ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_selopt_to_int_format.
*&---------------------------------------------------------------------*
*& Description: Converts select parameters in internal format
*&---------------------------------------------------------------------*
    TYPES lty_value_ref TYPE REF TO data.
    DATA lt_ref_table TYPE TABLE OF lty_value_ref.

    FIELD-SYMBOLS <lv_value> LIKE cv_value1.

    CHECK cv_value1 <> space OR cv_value2 <> space.

    CHECK: iv_tabname IS NOT INITIAL,
           iv_fieldname IS NOT INITIAL.

    "" build ref table
    lt_ref_table = VALUE #( ( REF #( cv_value1 ) )
                            ( REF #( cv_value2 ) ) ).

    DATA(lr_converter) = NEW cl_fobu_input_util( tabname   = iv_tabname
                                                 fieldname = iv_fieldname ).

    """ convert requested values into internal format - if possible.
    LOOP AT lt_ref_table ASSIGNING FIELD-SYMBOL(<lv_ref_value>).
      ASSIGN <lv_ref_value>->* TO <lv_value>.
      IF <lv_value> = space.
        CONTINUE.
      ENDIF.

      lr_converter->input_convert( EXPORTING  field_value_ext   = <lv_value>
                                   IMPORTING  field_value_int_c = <lv_value>
                                   EXCEPTIONS illegal_value     = 1 ).
      IF sy-subrc = 1.
        IF if_print_error_message = abap_true.
          zcl_sat_message_helper=>print_system_message( ).
        ELSE.
          DATA(lr_conversion_exc) = zcx_sat_conversion_exc=>create_from_sy( ).
          RAISE EXCEPTION lr_conversion_exc.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_values_to_disp_format.
*&---------------------------------------------------------------------*
*& Description: Converts input values to display format
*&---------------------------------------------------------------------*
    CHECK cv_value1 <> space OR cv_value2 <> space.

    DATA(lv_typename) = COND #(
      WHEN iv_rollname IS NOT INITIAL                 THEN iv_rollname
      WHEN iv_type = cl_abap_typedescr=>typekind_int1 THEN 'INT1'
      ELSE                                                 iv_type ).

    DATA(lr_converter) = NEW cl_fobu_input_util( typename = lv_typename
                                                 length   = iv_length
                                                 decimals = iv_decimals ).

    IF cv_value1 <> space.
      lr_converter->output_convert( EXPORTING  field_value_int = cv_value1
                                    IMPORTING  field_value_ext = cv_value1
                                    EXCEPTIONS OTHERS          = 1 ).
    ENDIF.

    IF cv_value2 <> space.
      lr_converter->output_convert( EXPORTING  field_value_int = cv_value2
                                    IMPORTING  field_value_ext = cv_value2
                                    EXCEPTIONS OTHERS          = 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_values_to_int_format.
*&---------------------------------------------------------------------*
*& Description: Converts select parameters in internal format
*&---------------------------------------------------------------------*
    TYPES lty_value_ref TYPE REF TO data.
    DATA lt_ref_table TYPE TABLE OF lty_value_ref.

    FIELD-SYMBOLS <lv_value> LIKE cv_value1.

    CHECK cv_value1 <> space OR cv_value2 <> space.

    "" build ref table
    lt_ref_table = VALUE #( ( REF #( cv_value1 ) )
                            ( REF #( cv_value2 ) ) ).

    DATA(lv_typename) = COND #(
      WHEN iv_rollname IS NOT INITIAL                 THEN iv_rollname
      WHEN iv_type = cl_abap_typedescr=>typekind_int1 THEN 'INT1'
      ELSE                                                 iv_type ).
    DATA(lr_converter) = NEW cl_fobu_input_util( typename = lv_typename
                                                 length   = iv_length
                                                 decimals = iv_decimals ).

    """ convert requested values into internal format - if possible.
    LOOP AT lt_ref_table ASSIGNING FIELD-SYMBOL(<lv_ref_value>).
      ASSIGN <lv_ref_value>->* TO <lv_value>.
      IF <lv_value> = space.
        CONTINUE.
      ENDIF.

      lr_converter->input_convert( EXPORTING  field_value_ext   = <lv_value>
                                   IMPORTING  field_value_int_c = <lv_value>
                                   EXCEPTIONS illegal_value     = 1 ).
      IF sy-subrc = 1.
        IF if_print_error_message = abap_true.
          zcl_sat_message_helper=>print_system_message( ).
        ELSE.
          DATA(lr_conversion_exc) = zcx_sat_conversion_exc=>create_from_sy( ).
          RAISE EXCEPTION lr_conversion_exc.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD perform_alpha_conversion_input.
*&---------------------------------------------------------------------*
*& Description: Performs alpha conversion of input data
*&---------------------------------------------------------------------*
    DATA lr_input TYPE REF TO data.
    FIELD-SYMBOLS <lv_input> TYPE any.

    " create type described by name
    DATA(lr_type_descr) = CAST cl_abap_datadescr(
        cl_abap_typedescr=>describe_by_name(
            COND string( WHEN iv_fieldname IS INITIAL THEN iv_tabname ELSE |{ iv_tabname }-{ iv_fieldname }| ) ) ).
    CREATE DATA lr_input TYPE HANDLE lr_type_descr.

    ASSIGN lr_input->* TO <lv_input>.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = iv_value
      IMPORTING output = <lv_input>.

    ev_output = <lv_input>.
  ENDMETHOD.
ENDCLASS.
