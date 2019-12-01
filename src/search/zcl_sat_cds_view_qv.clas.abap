"! <p class="shorttext synchronized" lang="en">Validator for CDS Search query</p>
CLASS zcl_sat_cds_view_qv DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_sat_general_qv
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS zif_sat_query_validator~validate_option
        REDEFINITION.
    METHODS zif_sat_query_validator~check_option_integrity
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gt_api_states TYPE RANGE OF string.
ENDCLASS.



CLASS zcl_sat_cds_view_qv IMPLEMENTATION.

  METHOD class_constructor.
    DATA(lt_api_states) = cl_ris_adt_res_release_states=>get_all( i_with_longtext = abap_false ).
    gt_api_states = VALUE #( FOR api IN lt_api_states ( sign = 'I' option = 'EQ' low = api-name ) ).
  ENDMETHOD.

  METHOD zif_sat_query_validator~validate_option.
    DATA: lf_invalid TYPE abap_bool.

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
    ).

*.. Remove exclusion characters before the actual validation
    DATA(lv_value) = iv_value.

    IF iv_option = zif_sat_c_object_search=>c_general_search_params-type.
      zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value ).
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
    ELSEIF iv_option = zif_sat_c_object_search=>c_general_search_params-release_state.
      zcl_sat_search_util=>remove_exclusion_string( CHANGING cv_value = lv_value ).

      lf_invalid = xsdbool( lv_value NOT IN gt_api_states ).

    ELSEIF iv_option = zif_sat_c_object_search=>c_cds_search_params-params.
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

  METHOD zif_sat_query_validator~check_option_integrity.
    IF line_exists( ct_options[ option = zif_sat_c_object_search=>c_cds_search_params-param ] ).
      DELETE ct_options WHERE option = zif_sat_c_object_search=>c_cds_search_params-params.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
