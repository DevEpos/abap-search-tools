"! <p class="shorttext synchronized">General Validator for Search Query</p>
CLASS zcl_sat_general_qv DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_query_validator.
    INTERFACES zif_sat_c_object_search.

    ALIASES validate FOR zif_sat_query_validator~validate_option.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA gt_api_states TYPE RANGE OF string.
ENDCLASS.


CLASS zcl_sat_general_qv IMPLEMENTATION.
  METHOD class_constructor.
    DATA(lt_api_states) = cl_ris_adt_res_release_states=>get_all( ).
    gt_api_states = VALUE #( FOR api IN lt_api_states ( sign = 'I' option = 'EQ' low = api-name ) ).
  ENDMETHOD.

  METHOD zif_sat_query_validator~validate_option.
    IF iv_value IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING textid = zcx_sat_object_search=>option_incomplete
                  msgv1  = |{ iv_option }|.
    ENDIF.

    CASE iv_option.

      WHEN zif_sat_c_object_search=>c_search_option-max_rows.
        IF iv_value CN '0123456789'.
          RAISE EXCEPTION TYPE zcx_sat_object_search
            EXPORTING textid = zcx_sat_object_search=>option_val_not_numeric
                      msgv1  = |{ iv_option }|.
        ENDIF.

        RETURN.
      WHEN zif_sat_c_object_search=>c_general_search_params-release_state.
        IF iv_value NOT IN gt_api_states.
          RAISE EXCEPTION TYPE zcx_sat_object_search
            EXPORTING textid = zcx_sat_object_search=>invalid_option_value
                      msgv1  = |{ iv_option }|
                      msgv2  = |{ iv_value }|.
        ENDIF.
        RETURN.

      WHEN zif_sat_c_object_search=>c_general_search_params-package.
        IF iv_value NA '*+'.
          SELECT SINGLE @abap_true
            FROM tdevc
            WHERE devclass = @iv_value
            INTO @DATA(lf_package_exists).
          IF lf_package_exists = abap_false.
            RAISE EXCEPTION TYPE zcx_sat_object_search
              EXPORTING textid = zcx_sat_object_search=>invalid_package
                        msgv1  = |{ iv_value }|.
          ENDIF.
        ENDIF.
    ENDCASE.

    " handle options with fixed content assist
    IF is_content_assist-assist_type = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item.
      IF NOT line_exists( is_content_assist-proposal_values[ name = iv_value ] ).
        RAISE EXCEPTION TYPE zcx_sat_object_search
          EXPORTING textid = zcx_sat_object_search=>invalid_option_value
                    msgv1  = |{ iv_option }|
                    msgv2  = |{ iv_value }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
