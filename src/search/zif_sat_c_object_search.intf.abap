"! <p class="shorttext synchronized" lang="en">Constants for Object search</p>
INTERFACE zif_sat_c_object_search
  PUBLIC .

  CONSTANTS:
    BEGIN OF c_search_type,
      cds_view        TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'CDS',
      db_tab_view     TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'DBTABVIEW',
      class_interface TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'CLASSINTF',
    END OF c_search_type.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">General Search options for object search</p>
    BEGIN OF c_search_option,
      by_owner       TYPE string VALUE 'OWNER',
      by_select_from TYPE string VALUE 'FROM',
      by_association TYPE string VALUE 'ASSOC',
      by_api         TYPE string VALUE 'API',
      by_description TYPE string VALUE 'DESC',
      by_type        TYPE string VALUE 'TYPE',
      by_package     TYPE string VALUE 'PACKAGE',
      by_anno        TYPE string VALUE 'ANNO',
      by_field       TYPE string VALUE 'FIELD',
      by_params      TYPE string VALUE 'PARAMS',
      by_param       TYPE string VALUE 'PARAM',
      max_rows       TYPE string VALUE 'MAXROWS',
      by_extensions  TYPE string VALUE 'EXTBY',
    END OF c_search_option.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options for class/interface object search</p>
    BEGIN OF c_class_intf_search_option,
*.... The following options are only for AIE so no convenient
*...... short form is needed
      attribute      TYPE string VALUE 'attribute',
      method         TYPE string VALUE 'method',
      super_type     TYPE string VALUE 'superType',
      sub_type       TYPE string VALUE 'subType',
      interface      TYPE string VALUE 'interface',
      friend         TYPE string VALUE 'friend',
      is             TYPE string VALUE 'is',
      isot           TYPE string VALUE 'isNot',
      exposure       TYPE string VALUE 'exposure',
      abap_lang      TYPE string VALUE 'abapLanguage',
      nw_release     TYPE string VALUE 'nwRelease',
      program_status TYPE string VALUE 'programStatus',
      ref_object     TYPE string VALUE 'refObject',
    END OF c_class_intf_search_option.

  CONSTANTS:
    BEGIN OF c_general_search_params,
      query                  TYPE string VALUE 'query',
      object_name            TYPE string VALUE 'objectName',
      object_type            TYPE string VALUE 'objectType',
      max_rows               TYPE string VALUE 'maxRows',
      user                   TYPE string VALUE 'userName',
      release_state          TYPE string VALUE 'releaseState',
      description            TYPE string VALUE 'description',
      type                   TYPE string VALUE 'type',
      package                TYPE string VALUE 'packageName',
      use_and_for_filters    TYPE string VALUE 'useAndForFilters',
      read_api_state         TYPE string VALUE 'withApiState',
      get_all_results        TYPE string VALUE 'getAllResults',
      read_package_hierarchy TYPE string VALUE 'withPackageHierarchy',
    END OF c_general_search_params.

  CONSTANTS:
    BEGIN OF c_cds_search_params,
      field             TYPE string VALUE 'fieldName',
      select_from       TYPE string VALUE 'selectSourceIn',
      association       TYPE string VALUE 'associatedIn',
      only_local_assocs TYPE string VALUE 'localDeclaredAssocOnly',
      annotation        TYPE string VALUE 'annotation',
      param             TYPE string VALUE 'param',
      params            TYPE string VALUE 'hasParams',
      extended_by       TYPE string VALUE 'extendedBy',
    END OF c_cds_search_params.

  CONSTANTS:
    BEGIN OF c_dbtab_search_params,
      field TYPE string VALUE 'fieldName',
    END OF c_dbtab_search_params.

  CONSTANTS:
    "! Values for API option
    BEGIN OF c_api_option_value,
      released      TYPE string VALUE 'RELEASED',
      deprecated    TYPE string VALUE 'DEPRECATED',
      custom_fields TYPE string VALUE 'ADD_CUSTOM_FIELDS',
    END OF c_api_option_value.

  CONSTANTS:
    "! Values for 'TYPE' option
    BEGIN OF c_type_option_value,
      function        TYPE string VALUE 'FUNCTION',
      hierarchy       TYPE string VALUE 'HIERARCHY',
      view            TYPE string VALUE 'VIEW',
      abstract_entity TYPE string VALUE 'ABSTRACT',
      custom_entity   TYPE string VALUE 'CUSTOM',
      table           TYPE string VALUE 'TABLE',
      extend          TYPE string VALUE 'EXTEND',
    END OF c_type_option_value.
ENDINTERFACE.
