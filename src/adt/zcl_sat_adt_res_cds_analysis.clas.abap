"! <p class="shorttext synchronized" lang="en">CDS Analysis for ADT</p>
CLASS zcl_sat_adt_res_cds_analysis DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_cds_view TYPE zsat_cds_view_name.
    DATA mf_usage_analysis TYPE abap_bool.
    DATA ms_result TYPE zsat_adt_element_info.
    DATA mf_with_associations TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Analyze dependency usages of CDS</p>
    METHODS analyze_cds_usages.
    "! <p class="shorttext synchronized" lang="en">Parse SELECT part of cds</p>
    METHODS parse_select_from.
ENDCLASS.



CLASS zcl_sat_adt_res_cds_analysis IMPLEMENTATION.

  METHOD get.
    mv_cds_view = zcl_sat_adt_res_util=>get_request_param_value(
        io_request       = request
        iv_param_name    = zif_sat_c_adt_utils=>c_cds_analysis_parameter-cds_name
        if_mandatory     = abap_true
    ).
    mf_usage_analysis = zcl_sat_adt_res_util=>get_request_param_value(
      io_request    = request
      iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-usage_analysis
    ).
    mf_with_associations = zcl_sat_adt_res_util=>get_request_param_value(
      io_request    = request
      iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-with_associations
    ).

    IF mf_usage_analysis = abap_true.
*.... Analyze cds dependencies and return compressed analysis of all
*..... the used data sources
      analyze_cds_usages( ).
    ELSE.
*.... Parse data sources of cds view
      parse_select_from( ).
    ENDIF.

    CHECK ms_result IS NOT INITIAL.

    response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_generic_eleminfo_res_ch( )
        data            = ms_result
    ).
  ENDMETHOD.


  METHOD analyze_cds_usages.
    FIELD-SYMBOLS: <lt_element_info> TYPE zsat_adt_element_info_t.

    SELECT SINGLE entityid,
                  ddlname,
                  developmentpackage,
                  createdby,
                  rawentityid,
                  description
      FROM zsat_i_cdsentity
      WHERE entityid = @mv_cds_view
         OR ddlname  = @mv_cds_view
    INTO @DATA(ls_root).

    DATA(ls_dependency_info) = zcl_sat_cds_dep_analyzer=>get_used_entities(
      iv_cds_view_name   = |{ ls_root-ddlname }|
      if_for_adt         = abap_true
    ).

    DATA(ls_root_adt_object) = zcl_sat_adt_util=>create_adt_uri(
        iv_type  = zif_sat_c_entity_type=>cds_view
        iv_name  = space
        iv_name2 = |{ ls_root-ddlname }|
    ).
    ms_result-name = ls_root-entityid.
    ms_result-owner = ls_root-createdby.
    ms_result-package = ls_root-developmentpackage.
    ms_result-raw_name = ls_root-rawentityid.
    ms_result-type = ls_root_adt_object-type.
    ms_result-uri = ls_root_adt_object-uri.
    ms_result-children = NEW zsat_adt_element_info_t( ).
    ASSIGN ms_result-children->* TO <lt_element_info>.

    LOOP AT ls_dependency_info-dependencies ASSIGNING FIELD-SYMBOL(<ls_dependency>).
      APPEND INITIAL LINE TO <lt_element_info> ASSIGNING FIELD-SYMBOL(<ls_usage>).

      <ls_usage> = CORRESPONDING #( <ls_dependency> MAPPING type = adt_type ).
      <ls_usage>-properties = VALUE #(
        ( key = 'OCCURRENCE'          value = <ls_dependency>-occurrence )
        ( key = 'USED_ENTITIES_COUNT' value = <ls_dependency>-used_entities_count )
        ( key = 'USED_JOIN_COUNT'     value = <ls_dependency>-used_join_count )
        ( key = 'USED_UNION_COUNT'    value = <ls_dependency>-used_union_count )
      ).
      IF <ls_dependency>-api_state IS NOT INITIAL.
        <ls_usage>-properties = VALUE #( BASE <ls_usage>-properties ( key = 'API_STATE' value = <ls_dependency>-api_state ) ).
      ENDIF.
      IF <ls_dependency>-source_type IS NOT INITIAL.
        <ls_usage>-properties = VALUE #( BASE <ls_usage>-properties ( key = 'SOURCE_TYPE' value = <ls_dependency>-source_type ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD parse_select_from.
    DATA(lo_cds_parser) = NEW zcl_sat_adt_cds_parser( iv_cds = mv_cds_view ).
    lo_cds_parser->parse_cds(
        if_select_part  = abap_true
        if_associations = mf_with_associations
    ).
    ms_result = lo_cds_parser->ms_select_element_info.
  ENDMETHOD.

ENDCLASS.
