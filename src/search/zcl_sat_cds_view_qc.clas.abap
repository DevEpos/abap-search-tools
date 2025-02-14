"! <p class="shorttext synchronized">Converter for Parameters of CDS View Search</p>
CLASS zcl_sat_cds_view_qc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_general_qc FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_sat_query_converter~convert_value REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_cds_view_qc IMPLEMENTATION.
  METHOD zif_sat_query_converter~convert_value.
    CASE iv_option.

      " Converts the external CDS Types to internal DDL Source types
      WHEN zif_sat_c_object_search=>c_general_search_params-type.

        CASE cv_value.
          WHEN zif_sat_c_os_cds_options=>c_type_option_value-extend.
            cv_value = zif_sat_c_cds_view_type=>extend.

          WHEN zif_sat_c_os_cds_options=>c_type_option_value-function.
            cv_value = zif_sat_c_cds_view_type=>table_function.

          WHEN zif_sat_c_os_cds_options=>c_type_option_value-hierarchy.
            cv_value = zif_sat_c_cds_view_type=>hierarchy.

          WHEN zif_sat_c_os_cds_options=>c_type_option_value-view.
            cv_value = zif_sat_c_cds_view_type=>view.

          WHEN zif_sat_c_os_cds_options=>c_type_option_value-abstract_entity.
            cv_value = zif_sat_c_cds_view_type=>abstract_entity.

          WHEN zif_sat_c_os_cds_options=>c_type_option_value-custom_entity.
            cv_value = zif_sat_c_cds_view_type=>custom_entity.

          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_sat_object_search
              EXPORTING textid = zcx_sat_object_search=>invalid_option_value
                        msgv1  = |{ cv_value }|
                        msgv2  = |{ iv_option }|.
        ENDCASE.

      WHEN OTHERS.
        super->zif_sat_query_converter~convert_value( EXPORTING iv_option = iv_option
                                                      CHANGING  cv_value  = cv_value
                                                                cv_value2 = cv_value2 ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
