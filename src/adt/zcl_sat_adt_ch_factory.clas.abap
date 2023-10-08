"! <p class="shorttext synchronized">Content Handler Factory for ADT tools</p>
CLASS zcl_sat_adt_ch_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! Create content handler for navigation targets</p>
    CLASS-METHODS create_nav_targets_ref_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! Create content handler for search result</p>
    CLASS-METHODS create_search_result_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! Creates content handler for Object Search Config
    CLASS-METHODS create_search_config_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! Creates content handler for Object Search Query Input
    CLASS-METHODS create_query_input_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! Creates content handler for CDS Top-Down Result
    CLASS-METHODS create_top_down_res_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! Creates content handler for CDS Used Entities result
    CLASS-METHODS create_used_ent_res_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! Creates content handler for ADT Object Reference
    CLASS-METHODS create_adt_obj_ref_res_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    CLASS-METHODS create_adtobjrefs_res_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    CLASS-METHODS create_field_entity_res_handlr
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    CLASS-METHODS create_where_used_in_cds_res_h
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.
ENDCLASS.


CLASS zcl_sat_adt_ch_factory IMPLEMENTATION.
  METHOD create_nav_targets_ref_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_NAV_TARGETS'
                                                      root_name = 'NAVIGATION_TARGETS' ).
  ENDMETHOD.

  METHOD create_search_result_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_SEARCH_RESULT'
                                                      root_name = 'OBJECT_SEARCH_RESULT' ).
  ENDMETHOD.

  METHOD create_search_config_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_SEARCH_CONFIG'
                                                      root_name = 'SEARCH_CONFIG' ).
  ENDMETHOD.

  METHOD create_query_input_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_OBJECT_SEARCH_QUERY_INPUT'
                                                      root_name = 'QUERY_INPUT' ).
  ENDMETHOD.

  METHOD create_top_down_res_handler.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_TOPDOWN_ANALYSIS_RESULT'
                                                      root_name = 'TOP_DOWN_RESULT' ).
  ENDMETHOD.

  METHOD create_used_ent_res_handler.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_CDS_USED_ENT_ANAL_RES'
                                                      root_name = 'USED_ENTITIES_RESULT' ).
  ENDMETHOD.

  METHOD create_adt_obj_ref_res_handler.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_ADT_OBJ_REF'
                                                      root_name = 'ADT_OBJ_REF' ).
  ENDMETHOD.

  METHOD create_adtobjrefs_res_handler.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_ADT_OBJ_REFS'
                                                      root_name = 'ADT_OBJ_REFS' ).
  ENDMETHOD.

  METHOD create_field_entity_res_handlr.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_ADT_ENTITY_FIELD_INFOS'
                                                      root_name = 'FIELD_INFO_RESULT' ).
  ENDMETHOD.

  METHOD create_where_used_in_cds_res_h.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_WHERE_USED_IN_CDS_RESULT'
                                                      root_name = 'WUSL_RESULT' ).
  ENDMETHOD.
ENDCLASS.
