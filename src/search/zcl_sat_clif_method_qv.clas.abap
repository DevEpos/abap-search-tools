"! <p class="shorttext synchronized">Validator for Class/Interface Method Search Options</p>
CLASS zcl_sat_clif_method_qv DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_clsintf_qv
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_validator~validate_option REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_clif_method_qv IMPLEMENTATION.
  METHOD zif_sat_query_validator~validate_option.
*    IF iv_target = zif_sat_c_object_search=>c_search_fields-object_filter_input_key.
      super->zif_sat_query_validator~validate_option( iv_option         = iv_option
                                                      is_content_assist = is_content_assist
                                                      iv_target         = iv_target
                                                      iv_value          = iv_value
                                                      iv_value2         = iv_value2 ).
*    ELSE.

*    ENDIF.
  ENDMETHOD.
ENDCLASS.
