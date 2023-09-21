"! <p class="shorttext synchronized" lang="en">Query Config for Dictionary Views</p>
CLASS zcl_sat_ddicview_qc DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_sat_general_qc
  CREATE PUBLIC .

  PUBLIC SECTION.
  methods zif_sat_query_converter~convert_value REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_ddicview_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.

      WHEN zif_sat_c_object_search=>c_general_search_params-maintenance.
        cv_value = zcl_sat_table_filter_mapper=>map_maintflag_to_int( cv_value ).

      WHEN zif_sat_c_object_search=>c_general_search_params-type.
        cv_value = SWITCH #( cv_value
                             WHEN zif_sat_c_object_search=>c_view_class-ext-database THEN
                               zif_sat_c_object_search=>c_view_class-int-database
                             WHEN zif_sat_c_object_search=>c_view_class-ext-help THEN
                               zif_sat_c_object_search=>c_view_class-int-help
                             WHEN zif_sat_c_object_search=>c_view_class-ext-maintenance THEN
                               zif_sat_c_object_search=>c_view_class-int-maintenance
                             WHEN zif_sat_c_object_search=>c_view_class-ext-projection THEN
                               zif_sat_c_object_search=>c_view_class-int-projection ).

      WHEN OTHERS.
        super->zif_sat_query_converter~convert_value( EXPORTING iv_option = iv_option
                                                                iv_target = iv_target
                                                      CHANGING  cv_value  = cv_value
                                                                cv_value2 = cv_value2 ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
