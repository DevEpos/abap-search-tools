"! <p class="shorttext synchronized">Query Converter for Structure Search</p>
CLASS zcl_sat_struct_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_struct_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.
      WHEN zif_sat_c_os_tabl_options=>c_filter_key-enhancement_category.
        cv_value = zcl_sat_table_filter_mapper=>map_enhanccat_to_int( cv_value ).

      WHEN OTHERS.
        super->zif_sat_query_converter~convert_value( EXPORTING iv_option = iv_option
                                                                iv_target = iv_target
                                                      CHANGING  cv_value  = cv_value
                                                                cv_value2 = cv_value2 ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
