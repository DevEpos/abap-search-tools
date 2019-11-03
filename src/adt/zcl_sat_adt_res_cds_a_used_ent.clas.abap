"! <p class="shorttext synchronized" lang="en">Resource for CDS Used Entities Analysis</p>
CLASS zcl_sat_adt_res_cds_a_used_ent DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_cds_analysis
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_internal
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_cds_a_used_ent IMPLEMENTATION.
  METHOD get_internal.
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

ENDCLASS.
