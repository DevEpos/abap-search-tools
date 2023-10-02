"! <p class="shorttext synchronized">Base Converter for Object Search Query</p>
CLASS zcl_sat_general_qc DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_query_converter.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_general_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    IF    iv_option = zif_sat_c_object_search=>c_general_search_params-user
       OR iv_option = zif_sat_c_object_search=>c_general_search_params-changed_by.
      IF cv_value = 'ME'.
        cv_value = sy-uname.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
