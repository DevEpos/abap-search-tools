"! <p class="shorttext synchronized">Converter for parameters of DTEL search</p>
CLASS zcl_sat_dtel_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_dtel_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.
      WHEN zif_sat_c_os_dtel_options=>c_filter_key-length.
        es_range = convert_to_number_range( iv_option = iv_option
                                            iv_length = 6
                                            iv_sign   = iv_sign
                                            iv_value  = cv_value ).

      WHEN zif_sat_c_os_dtel_options=>c_filter_key-ref_type.
        cv_value = SWITCH #( cv_value
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-built_in THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-built_in
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-class THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-class
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-interface THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-interface
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-data THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-data
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-object THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-object
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-structured THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-structured
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-data_element THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-data_element
                             WHEN zif_sat_c_os_dtel_options=>c_ref_type-ext-table_type THEN
                               zif_sat_c_os_dtel_options=>c_ref_type-int-table_type ).
      WHEN zif_sat_c_os_dtel_options=>c_filter_key-type_category.
        cv_value = SWITCH #( cv_value
                             WHEN zif_sat_c_os_dtel_options=>c_type_category-ext-domain THEN
                               zif_sat_c_os_dtel_options=>c_type_category-int-domain
                             WHEN zif_sat_c_os_dtel_options=>c_type_category-ext-predefined THEN
                               zif_sat_c_os_dtel_options=>c_type_category-int-predefined
                             WHEN zif_sat_c_os_dtel_options=>c_type_category-ext-ref_type THEN
                               zif_sat_c_os_dtel_options=>c_type_category-int-ref_type ).
      WHEN OTHERS.
        super->zif_sat_query_converter~convert_value( EXPORTING iv_sign   = iv_sign
                                                                iv_sign2  = iv_sign2
                                                                iv_option = iv_option
                                                      IMPORTING es_range  = es_range
                                                      CHANGING  cv_value  = cv_value
                                                                cv_value2 = cv_value2 ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
