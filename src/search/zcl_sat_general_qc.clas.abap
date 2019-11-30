"! <p class="shorttext synchronized" lang="en">Base Converter for Object Search Query</p>
CLASS zcl_sat_general_qc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_query_converter.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_general_qc IMPLEMENTATION.

  METHOD zif_sat_query_converter~convert_value.
    IF iv_option = zif_sat_c_object_search=>c_general_search_params-user.
      IF cv_value = 'ME'.
        cv_value = sy-uname.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
