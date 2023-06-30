"! <p class="shorttext synchronized">Content Handler Factory for ADT tools</p>
CLASS zcl_sat_adt_ch_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Create content handler for CDS View element info</p>
    CLASS-METHODS create_cds_elem_info_res_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for DB Table element info</p>
    CLASS-METHODS create_table_elem_info_res_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for DB View element info</p>
    CLASS-METHODS create_view_elem_info_res_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for CDS secondary elements</p>
    CLASS-METHODS create_cds_secondary_res_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for generic element info</p>
    CLASS-METHODS create_generic_eleminfo_res_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for generic element infos</p>
    CLASS-METHODS create_gen_eleminfos_res_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for navigation targets</p>
    CLASS-METHODS create_nav_targets_ref_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Create content handler for search result</p>
    CLASS-METHODS create_search_result_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    CLASS-METHODS create_search_config_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    CLASS-METHODS create_query_input_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.
ENDCLASS.


CLASS zcl_sat_adt_ch_factory IMPLEMENTATION.
  METHOD create_cds_elem_info_res_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_CDS_VIEW_ELEMENT_INFO'
                                                      root_name = 'CDS_VIEW_ELEMENT_INFO' ).
  ENDMETHOD.

  METHOD create_table_elem_info_res_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_TABLE_ELEMENT_INFO'
                                                      root_name = 'TABLE_INFO' ).
  ENDMETHOD.

  METHOD create_view_elem_info_res_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_VIEW_ELEMENT_INFO'
                                                      root_name = 'VIEW_INFO' ).
  ENDMETHOD.

  METHOD create_cds_secondary_res_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_CDS_SEC_ELEMENT_INFO'
                                                      root_name = 'CDS_SECONDARY_ELEMENTS' ).
  ENDMETHOD.

  METHOD create_generic_eleminfo_res_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_ELEMENT_INFO'
                                                      root_name = 'ELEMENT_INFO' ).
  ENDMETHOD.

  METHOD create_gen_eleminfos_res_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZSAT_ELEMENT_INFOS'
                                                      root_name = 'ELEMENT_INFOS' ).
  ENDMETHOD.

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
ENDCLASS.
