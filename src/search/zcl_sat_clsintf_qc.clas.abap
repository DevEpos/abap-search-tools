"! <p class="shorttext synchronized">Converter for Parameters of Class/Interface search</p>
CLASS zcl_sat_clsintf_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_clsintf_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.

      WHEN zif_sat_c_object_search=>c_class_intf_search_option-category.
        cv_value = zcl_sat_clif_search_param_util=>convert_category_to_int( iv_external = cv_value ).

      WHEN OTHERS.
        super->zif_sat_query_converter~convert_value( EXPORTING iv_option = iv_option
                                                      CHANGING  cv_value  = cv_value
                                                                cv_value2 = cv_value2 ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
