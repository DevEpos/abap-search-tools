"! <p class="shorttext synchronized">Resource for CDS Used Entities Analysis</p>
CLASS zcl_sat_adt_res_cds_a_used_ent DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_cds_analysis
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA ms_result TYPE zif_sat_ty_adt_types=>ty_cds_used_entities_result.

    METHODS execute_analysis.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_a_used_ent IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).
    execute_analysis( ).

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_used_ent_res_handler( )
                             data            = ms_result ).
  ENDMETHOD.

  METHOD execute_analysis.
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

    DATA(ls_dependency_info) = zcl_sat_cds_dep_analyzer=>get_used_entities( iv_cds_view_name = |{ ls_root-ddlname }|
                                                                            if_for_adt       = abap_true ).

    DATA(ls_root_adt_object) = zcl_sat_adt_util=>create_adt_uri( iv_type  = zif_sat_c_entity_type=>cds_view
                                                                 iv_name  = space
                                                                 iv_name2 = |{ ls_root-ddlname }| ).

    ms_result-source_entity = VALUE #( name     = ls_root-entityid
                                       owner    = ls_root-createdby
                                       devclass = ls_root-developmentpackage
                                       alt_name = ls_root-rawentityid
                                       type     = ls_root_adt_object-type
                                       uri      = ls_root_adt_object-uri ).

    LOOP AT ls_dependency_info-dependencies REFERENCE INTO DATA(lr_dependency).
      DATA(ls_used_entity) = VALUE zif_sat_ty_adt_types=>ty_cds_used_entity(
                                       entity_ref = VALUE #( name        = lr_dependency->name
                                                             alt_name    = lr_dependency->raw_name
                                                             devclass    = lr_dependency->package
                                                             type        = lr_dependency->adt_type
                                                             description = lr_dependency->description
                                                             uri         = lr_dependency->uri )
                                       usage_info = VALUE #( occurrence   = lr_dependency->occurrence
                                                             join_count   = lr_dependency->used_join_count
                                                             entity_count = lr_dependency->used_entities_count
                                                             union_count  = lr_dependency->used_union_count ) ).

      IF lr_dependency->api_state IS NOT INITIAL.
        ls_used_entity-entity_ref-properties = VALUE #( BASE ls_used_entity-entity_ref-properties
                                                        ( key   = 'API_STATE'
                                                          value = lr_dependency->api_state ) ).
      ENDIF.

      IF lr_dependency->source_type IS NOT INITIAL.
        ls_used_entity-entity_ref-properties = VALUE #( BASE ls_used_entity-entity_ref-properties
                                                        ( key   = 'SOURCE_TYPE'
                                                          value = lr_dependency->source_type ) ).
      ENDIF.

      ms_result-used_entities = VALUE #( BASE ms_result-used_entities
                                         ( ls_used_entity ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
