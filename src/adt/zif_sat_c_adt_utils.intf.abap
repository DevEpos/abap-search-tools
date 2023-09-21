"! <p class="shorttext synchronized">Constants for ADT Tools of DB Browser</p>
INTERFACE zif_sat_c_adt_utils
  PUBLIC.

  "! <p class="shorttext synchronized">URI segment for positional DDLS URI</p>
  CONSTANTS c_ddl_pos_uri_segment TYPE string VALUE '/source/main#start='.

  CONSTANTS:
    BEGIN OF c_element_info_parameter,
      uri         TYPE string VALUE 'objectUri',
      name        TYPE string VALUE 'objectName',
      object_type TYPE string VALUE 'objectType',
      basic_info  TYPE string VALUE 'basicInfoOnly',
    END OF c_element_info_parameter.
  CONSTANTS:
    BEGIN OF c_property_type,
      bool TYPE string VALUE 'bool',
    END OF c_property_type.
  CONSTANTS:
    BEGIN OF c_cds_analysis_parameter,
      cds_name          TYPE string VALUE 'cdsViewName',
      with_associations TYPE string VALUE 'withAssociations',
      usage_analysis    TYPE string VALUE 'usageAnalysis',
      source_origin     TYPE string VALUE 'sourceOrigin',
      entity_name       TYPE string VALUE 'entityName',
      recursive_search  TYPE string VALUE 'recursiveSearch',
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
ENDINTERFACE.
