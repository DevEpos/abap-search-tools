"! <p class="shorttext synchronized">Query Config for Dictionary Views</p>
CLASS zcl_sat_ddicview_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

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
                             WHEN zif_sat_c_os_view_options=>c_view_class-ext-database THEN
                               zif_sat_c_os_view_options=>c_view_class-int-database
                             WHEN zif_sat_c_os_view_options=>c_view_class-ext-help THEN
                               zif_sat_c_os_view_options=>c_view_class-int-help
                             WHEN zif_sat_c_os_view_options=>c_view_class-ext-maintenance THEN
                               zif_sat_c_os_view_options=>c_view_class-int-maintenance
                             WHEN zif_sat_c_os_view_options=>c_view_class-ext-projection THEN
                               zif_sat_c_os_view_options=>c_view_class-int-projection
                             WHEN zif_sat_c_os_view_options=>c_view_class-ext-external THEN
                               zif_sat_c_os_view_options=>c_view_class-int-external ).

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
