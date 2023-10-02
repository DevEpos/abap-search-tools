"! <p class="shorttext synchronized">Validator for Class/Interface Method Search Options</p>
CLASS zcl_sat_clif_method_qv DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qv
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_validator~validate_option REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_clif_qv TYPE REF TO zif_sat_query_validator.
ENDCLASS.


CLASS zcl_sat_clif_method_qv IMPLEMENTATION.
  METHOD zif_sat_query_validator~validate_option.
    IF iv_target = zif_sat_c_object_search=>c_search_fields-object_filter_input_key.
      IF mo_clif_qv IS INITIAL.
        mo_clif_qv = NEW zcl_sat_clsintf_qv( ).
      ENDIF.
      mo_clif_qv->validate_option( iv_option         = iv_option
                                   is_content_assist = is_content_assist
                                   iv_value          = iv_value
                                   iv_value2         = iv_value2 ).
    ELSE.
      " no special handling for method param necessary at this time
      super->zif_sat_query_validator~validate_option( iv_option         = iv_option
                                                      is_content_assist = is_content_assist
                                                      iv_value          = iv_value
                                                      iv_value2         = iv_value2 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
