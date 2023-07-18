"! <p class="shorttext synchronized">Validator for Class/Interface Search</p>
CLASS zcl_sat_clsintf_qv DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qv
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_validator~validate_option REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_clsintf_qv IMPLEMENTATION.
  METHOD zif_sat_query_validator~validate_option.
    DATA lf_invalid TYPE abap_bool.

    super->zif_sat_query_validator~validate_option( iv_option         = iv_option
                                                    is_content_assist = is_content_assist
                                                    iv_value          = iv_value
                                                    iv_value2         = iv_value2 ).

    IF iv_target <> zif_sat_c_object_search=>c_search_fields-object_filter_input_key.
      RETURN.
    ENDIF.

    CASE iv_option.

      WHEN zif_sat_c_object_search=>c_class_intf_search_option-category.
        TRY.
            zcl_sat_clif_search_param_util=>convert_category_to_int( iv_external = iv_value ).
          CATCH zcx_sat_conversion_exc.
            lf_invalid = abap_true.
        ENDTRY.

      WHEN zif_sat_c_object_search=>c_class_intf_search_option-flag.
        CASE iv_value.

          WHEN zif_sat_c_object_search=>c_class_intf_flags-has_test OR
               zif_sat_c_object_search=>c_class_intf_flags-is_abstract OR
               zif_sat_c_object_search=>c_class_intf_flags-is_final OR
               zif_sat_c_object_search=>c_class_intf_flags-is_fixpoint OR
               zif_sat_c_object_search=>c_class_intf_flags-is_shared_memory or
               zif_sat_c_object_search=>c_class_intf_flags-has_unicode_checks.

          WHEN OTHERS.
            lf_invalid = abap_true.
        ENDCASE.
    ENDCASE.

    IF lf_invalid = abap_true.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING textid = zcx_sat_object_search=>invalid_option_value
                  msgv1  = |{ iv_option }|
                  msgv2  = |{ iv_value }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
