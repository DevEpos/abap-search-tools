"! <p class="shorttext synchronized">Validator for Database Table/View Search query</p>
CLASS zcl_sat_dbtab_qv DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qv
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_validator~validate_option REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_dbtab_qv IMPLEMENTATION.
  METHOD zif_sat_query_validator~validate_option.
    super->validate( iv_option         = iv_option
                     is_content_assist = is_content_assist
                     iv_value          = iv_value
                     iv_value2         = iv_value2 ).
  ENDMETHOD.
ENDCLASS.
