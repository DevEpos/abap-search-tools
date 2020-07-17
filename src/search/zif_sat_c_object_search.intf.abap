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
      attribute  TYPE string VALUE 'attribute',
      method     TYPE string VALUE 'method',
      super_type TYPE string VALUE 'superType',
      interface  TYPE string VALUE 'interface',
      "! Category of the class/interface (see data element SEOCATEGRY)
      "! --> check against current NW stack to get valid options
      category   TYPE string VALUE 'category',
      friend     TYPE string VALUE 'friend',
      "! Checks for the following criteria
      "! <ul>
      "!   <li>abstract</li>
      "!   <li>final</li>
      "!   <li>test</li>
      "!   <li>fixpoint</li>
      "!   <li>shared_memory</li>
      "! </ul>
      flag       TYPE string VALUE 'flag',
      "! Exposure (for methods/attributes). Possible values are
      "! <ul>
      "!   <li>private</li>
      "!   <li>protected</li>
      "!   <li>public</li>
      "! </ul>
      exposure   TYPE string VALUE 'exposure',
      "! Holds the name of re
      ref_object TYPE string VALUE 'refObject',
    END OF c_class_intf_search_option.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Flags for Class/Interface</p>
    BEGIN OF c_class_intf_flags,
      is_abstract        TYPE string VALUE 'ABSTRACT',
      is_final           TYPE string VALUE 'FINAL',
      has_test           TYPE string VALUE 'TEST',
      is_fixpoint        TYPE string VALUE 'FIXPOINT',
      is_shared_memory   TYPE string VALUE 'SHARED_MEMORY',
      has_unicode_checks TYPE string VALUE 'UNICODE',
    END OF c_class_intf_flags.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Categories of ABAP OO Class</p>
    BEGIN OF c_class_categories,
      general      TYPE string VALUE 'GENERAL',
      exit         TYPE string VALUE 'EXIT',
      persistent   TYPE string VALUE 'PERSISTENT',
      pers_factory TYPE string VALUE 'PERS_FACTORY',
      exception    TYPE string VALUE 'EXCEPTION',
      test_class   TYPE string VALUE 'TEST_CLASS',
      area_class   TYPE string VALUE 'AREA_CLASS',
      wd_runtime   TYPE string VALUE 'WD_RUNTIME',
      behavior     TYPE string VALUE 'BEHAVIOR',
    END OF c_class_categories.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">ABAP Language versions</p>
    BEGIN OF c_abap_lang_versions,
      unicode                 TYPE string VALUE 'UNICODE',
      key_user                TYPE string VALUE 'KEY_USER',
      static_abap_limited_use TYPE string VALUE 'STATIC_ABAP_LIMITED_USE',
      std_abap_limited_use    TYPE string VALUE 'STD_ABAP_LIMITED_USE',
      cloud_platform          TYPE string VALUE 'CLOUD_PLATFORM',
      non_unicode             TYPE string VALUE 'NON_UNICODE',
    END OF c_abap_lang_versions.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">General search options</p>
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
    "! <p class="shorttext synchronized" lang="en">Search options for CDS Search</p>
    BEGIN OF c_cds_search_params,
      field             TYPE string VALUE 'fieldName',
      select_from       TYPE string VALUE 'selectSourceIn',
      association       TYPE string VALUE 'associatedIn',
      annotation        TYPE string VALUE 'annotation',
      param             TYPE string VALUE 'param',
      params            TYPE string VALUE 'hasParams',
      extended_by       TYPE string VALUE 'extendedBy',
    END OF c_cds_search_params.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Types for class/interface</p>
    BEGIN OF c_class_types,
      class     TYPE string VALUE 'CLAS',
      interface TYPE string VALUE 'INTF',
    END OF c_class_types.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options DB search</p>
    BEGIN OF c_dbtab_search_params,
      field          TYPE string VALUE 'fieldName',
      delivery_class TYPE string VALUE 'deliveryClass',
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
