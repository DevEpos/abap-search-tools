"! <p class="shorttext synchronized" lang="en">Validator for CDS Search query</p>
CLASS zcl_sat_cds_view_qv DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_sat_general_qv
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_sat_query_validator~validate_option
        REDEFINITION.
    METHODS zif_sat_query_validator~check_option_integrity
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_cds_view_qv IMPLEMENTATION.

  METHOD zif_sat_query_validator~validate_option.
    DATA: lf_invalid TYPE abap_bool.

    super->validate(
        iv_option = iv_option
        iv_value  = iv_value
        iv_value2 = iv_value2
    ).

    IF iv_option = zif_sat_c_object_search=>c_general_search_params-type.
      CASE iv_value.

        WHEN zif_sat_c_object_search=>c_type_option_value-function OR
             zif_sat_c_object_search=>c_type_option_value-hierarchy OR
             zif_sat_c_object_search=>c_type_option_value-abstract_entity OR
             zif_sat_c_object_search=>c_type_option_value-custom_entity OR
             zif_sat_c_object_search=>c_type_option_value-view OR
             zif_sat_c_object_search=>c_type_option_value-view_entity OR
             zif_sat_c_object_search=>c_type_option_value-extend or
             zif_sat_c_object_search=>c_type_option_value-extend2.

        WHEN OTHERS.
          lf_invalid = abap_true.
      ENDCASE.
    ELSEIF iv_option = zif_sat_c_object_search=>c_cds_search_params-params.
      IF iv_value <> 'TRUE' AND iv_value <> 'FALSE'.
        lf_invalid = abap_true.
      ENDIF.
    ENDIF.

    IF lf_invalid = abap_true.
      RAISE EXCEPTION TYPE zcx_sat_object_search
        EXPORTING
          textid = zcx_sat_object_search=>invalid_option_value
          msgv1  = |{ iv_option }|
          msgv2  = |{ iv_value }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_sat_query_validator~check_option_integrity.
    IF line_exists( ct_options[ option = zif_sat_c_object_search=>c_cds_search_params-param ] ).
      DELETE ct_options WHERE option = zif_sat_c_object_search=>c_cds_search_params-params.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
