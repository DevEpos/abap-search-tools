"! <p class="shorttext synchronized" lang="en">Constants for ADT Tools of DB Browser</p>
INTERFACE zif_sat_c_adt_utils
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">URI segment for positional DDLS URI</p>
  CONSTANTS c_ddl_pos_uri_segment TYPE string VALUE '/source/main#start='.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">IDs of ADT Resource Handlers</p>
    BEGIN OF c_resource_handler,
      object_search                 TYPE string VALUE 'ZCL_SAT_ADT_RES_OBJECT_SEARCH',
      object_search_cds             TYPE string VALUE 'ZCL_SAT_ADT_RES_OBJS_CDS',
      element_info                  TYPE string VALUE 'ZCL_SAT_ADT_RES_ELEMENT_INFO',
      element_info_by_uri           TYPE string VALUE 'ZCL_SAT_ADT_RES_ELEMINFO_BYURI',
      cds_secondary_element_info    TYPE string VALUE 'ZCL_SAT_ADT_RES_CDS_SECELINFO',
      cds_fields                    TYPE string VALUE 'ZCL_SAT_ADT_RES_CDSFIELD_VH',
      db_table_fields               TYPE string VALUE 'ZCL_SAT_ADT_RES_TABFIELD_VH',
      annotations                   TYPE string VALUE 'ZCL_SAT_ADT_RES_ANNO_VH',
      annotation_values             TYPE string VALUE 'ZCL_SAT_ADT_RES_ANNO_VALUE_VH',
      database_entities             TYPE string VALUE 'ZCL_SAT_ADT_RES_DB_ENTITY_VH',
      release_api_states            TYPE string VALUE 'CL_RIS_ADT_RES_RELEASE_STATES',
      cds_types                     TYPE string VALUE 'ZCL_SAT_ADT_RES_CDS_TYPE_VH',
      table_types                   TYPE string VALUE 'ZCL_SAT_ADT_RES_TABLE_TYPE_VH',
      cds_extensions                TYPE string VALUE 'ZCL_SAT_ADT_RES_CDS_EXT_VH',
      sapaox_launcher               TYPE string VALUE 'ZCL_SAT_ADT_RES_AOX_LAUNCHER',
      cds_analysis                  TYPE string VALUE 'ZCL_SAT_ADT_RES_CDS_ANALYSIS',
      cds_top_down_analysis         TYPE string VALUE 'ZCL_SAT_ADT_RES_CDS_A_TOPDOWN',
      cds_used_entities_analysis    TYPE string VALUE 'ZCL_SAT_ADT_RES_CDS_A_USED_ENT',
      navigation_targets            TYPE string VALUE 'ZCL_SAT_ADT_RES_NAV_TARGETS',
      column_info                   TYPE string VALUE 'ZCL_SAT_ADT_RES_COLUMN_INFO',
      column_hierarchy              TYPE string VALUE 'ZCL_SAT_ADT_RES_COL_HIERARCHY',
      column_where_used_list        TYPE string VALUE 'ZCL_SAT_ADT_RES_COL_WHERE_USED',
      ddic_repo_access              TYPE string VALUE 'ZCL_SAT_ADT_RES_DDIC_REP_ACC',
    END OF c_resource_handler.

  CONSTANTS:
    BEGIN OF c_element_info_parameter,
      uri         TYPE string VALUE 'objectUri',
      name        TYPE string VALUE 'objectName',
      object_type TYPE string VALUE 'objectType',
      basic_info  TYPE string VALUE 'basicInfoOnly',
    END OF c_element_info_parameter.
  CONSTANTS:
    BEGIN OF c_cds_analysis_parameter,
      cds_name          TYPE string VALUE 'cdsViewName',
      with_associations TYPE string VALUE 'withAssociations',
      usage_analysis    TYPE string VALUE 'usageAnalysis',
    END OF c_cds_analysis_parameter.
  CONSTANTS:
    BEGIN OF c_cds_elem_info_parameter,
      show_association_name TYPE string VALUE 'showAssocName',
    END OF c_cds_elem_info_parameter.

  CONSTANTS:
    BEGIN OF c_ddic_repo_access_params,
      "! Access Mode for Respository access <br>
      "! Possible options are:
      "! <ul>
      "! <li>getUriFromPaths</li>
      "! <li>getFields</li>
      "! </ul>
      access_mode TYPE string VALUE 'accessMode',
      uri         TYPE string VALUE 'uri',
      paths       TYPE string VALUE 'path',
      "! List of optional filters to be used during
      "! the DDIC Access <br>
      "! Use to the following filter pattern: <key>:<value>
      filters     TYPE string VALUE 'filter',
    END OF c_ddic_repo_access_params.
  CONSTANTS:
    BEGIN OF c_db_fields_info_parameter,
      name               TYPE string VALUE 'name',
      type               TYPE string VALUE 'type',
      search_calc_fields TYPE string VALUE 'searchCalcFields',
      search_db_views    TYPE string VALUE 'searchDbViewUsages',
      field              TYPE string VALUE 'field',
    END OF c_db_fields_info_parameter.

  CONSTANTS:
    BEGIN OF c_adt_types,
      data_definition  TYPE string VALUE 'DDLS/DF',
      table_definition TYPE string VALUE 'TABL/DT',
      view_definition  TYPE string VALUE 'VIEW/DV',
    END OF c_adt_types.
ENDINTERFACE.