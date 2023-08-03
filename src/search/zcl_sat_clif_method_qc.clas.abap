"! <p class="shorttext synchronized">Converter for Params of Method Search</p>
CLASS zcl_sat_clif_method_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_class_qc TYPE REF TO zif_sat_query_converter.
ENDCLASS.


CLASS zcl_sat_clif_method_qc IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_class_qc = NEW zcl_sat_clsintf_qc( ).
  ENDMETHOD.

  METHOD zif_sat_query_converter~convert_value.
    IF iv_target = zif_sat_c_object_search=>c_search_fields-object_filter_input_key.
      mo_class_qc->convert_value( EXPORTING iv_option = iv_option
                                  CHANGING  cv_value  = cv_value
                                            cv_value2 = cv_value2 ).
    ELSE.
      CASE iv_option.
        WHEN zif_sat_c_object_search=>c_method_search_option-level.
          cv_value = SWITCH #( cv_value
                               WHEN zif_sat_c_object_search=>c_method_level-instance THEN '0'
                               WHEN zif_sat_c_object_search=>c_method_level-static   THEN '1' ).

        WHEN zif_sat_c_object_search=>c_method_search_option-visibility.
          cv_value = SWITCH #( cv_value
                               WHEN zif_sat_c_object_search=>c_visibility-private   THEN '0'
                               WHEN zif_sat_c_object_search=>c_visibility-protected THEN '1'
                               WHEN zif_sat_c_object_search=>c_visibility-public    THEN '2' ).

        WHEN zif_sat_c_object_search=>c_general_search_params-type.
          cv_value = SWITCH #( cv_value
                               WHEN zif_sat_c_object_search=>c_method_types-general            THEN '0'
                               WHEN zif_sat_c_object_search=>c_method_types-event_handler      THEN '1'
                               WHEN zif_sat_c_object_search=>c_method_types-constructor        THEN '2'
                               WHEN zif_sat_c_object_search=>c_method_types-virtual_getter     THEN '4'
                               WHEN zif_sat_c_object_search=>c_method_types-virtual_setter     THEN '5'
                               WHEN zif_sat_c_object_search=>c_method_types-test               THEN '6'
                               WHEN zif_sat_c_object_search=>c_method_types-cds_table_function THEN '7'
                               WHEN zif_sat_c_object_search=>c_method_types-amdp_ddl_object    THEN '8' ).

        WHEN zif_sat_c_object_search=>c_method_search_option-status.
          cv_value = SWITCH #( cv_value
                               WHEN zif_sat_c_object_search=>c_method_status-standard THEN
                                 zif_sat_c_object_search=>c_method_status_int-standard
                               WHEN zif_sat_c_object_search=>c_method_status-implemented THEN
                                 zif_sat_c_object_search=>c_method_status_int-implemented
                               WHEN zif_sat_c_object_search=>c_method_status-redefined THEN
                                 zif_sat_c_object_search=>c_method_status_int-redefined ).
        WHEN OTHERS.
          super->zif_sat_query_converter~convert_value( EXPORTING iv_option = iv_option
                                                        CHANGING  cv_value  = cv_value
                                                                  cv_value2 = cv_value2 ).
      ENDCASE.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
