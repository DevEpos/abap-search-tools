"! <p class="shorttext synchronized">Query Config for Database Tables</p>
CLASS zcl_sat_dbtab_qc DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_sat_general_qc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_dbtab_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.
      WHEN zif_sat_c_object_search=>c_dbtab_search_params-enhancement_category.
        cv_value = zcl_sat_table_filter_mapper=>map_enhanccat_to_int( cv_value ).

      WHEN zif_sat_c_object_search=>c_general_search_params-maintenance.
        cv_value = zcl_sat_table_filter_mapper=>map_maintflag_to_int( cv_value ).

      WHEN zif_sat_c_object_search=>c_dbtab_search_params-buffering_type.
        cv_value = SWITCH #( cv_value
                             WHEN zif_sat_c_object_search=>c_db_buffer_type-ext-full_table THEN
                               zif_sat_c_object_search=>c_db_buffer_type-int-full_table
                             WHEN zif_sat_c_object_search=>c_db_buffer_type-ext-no_buffering THEN
                               zif_sat_c_object_search=>c_db_buffer_type-int-no_buffering
                             WHEN zif_sat_c_object_search=>c_db_buffer_type-ext-single_entries THEN
                               zif_sat_c_object_search=>c_db_buffer_type-int-single_entries
                             WHEN zif_sat_c_object_search=>c_db_buffer_type-ext-full_with_gen_key THEN
                               zif_sat_c_object_search=>c_db_buffer_type-int-full_with_gen_key ).

      WHEN zif_sat_c_object_search=>c_dbtab_search_params-buffering.
        cv_value = SWITCH #( cv_value
                             WHEN zif_sat_c_object_search=>c_db_buffer_status-ext-allowed_but_off THEN
                               zif_sat_c_object_search=>c_db_buffer_status-int-allowed_but_off
                             WHEN zif_sat_c_object_search=>c_db_buffer_status-ext-off THEN
                               zif_sat_c_object_search=>c_db_buffer_status-int-off
                             WHEN zif_sat_c_object_search=>c_db_buffer_status-ext-on THEN
                               zif_sat_c_object_search=>c_db_buffer_status-int-on ).

      WHEN zif_sat_c_object_search=>c_dbtab_search_params-storage_type.
        cv_value = SWITCH #( cv_value
                             WHEN zif_sat_c_object_search=>c_table_storage_type-ext-column THEN
                               zif_sat_c_object_search=>c_table_storage_type-int-column
                             WHEN zif_sat_c_object_search=>c_table_storage_type-ext-row THEN
                               zif_sat_c_object_search=>c_table_storage_type-int-row
                             WHEN zif_sat_c_object_search=>c_table_storage_type-ext-undefined THEN
                               zif_sat_c_object_search=>c_table_storage_type-int-undefined ).

      WHEN OTHERS.
        super->zif_sat_query_converter~convert_value( EXPORTING iv_option = iv_option
                                                                iv_target = iv_target
                                                      CHANGING  cv_value  = cv_value
                                                                cv_value2 = cv_value2 ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
