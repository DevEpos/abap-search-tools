"! <p class="shorttext synchronized">Converter for parameters of DOMA search</p>
CLASS zcl_sat_doma_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_doma_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.
      WHEN zif_sat_c_os_doma_options=>c_filter_key-length OR
           zif_sat_c_os_doma_options=>c_filter_key-outlength OR
           zif_sat_c_os_doma_options=>c_filter_key-decimals.
        es_range = convert_to_number_range( iv_option = iv_option
                                            iv_length = 6
                                            iv_sign   = iv_sign
                                            iv_value  = cv_value ).

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
