"! <p class="shorttext synchronized">Constants for Object search</p>
INTERFACE zif_sat_c_object_search
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_search_type,
      cds_view        TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'cds',
      db_tab_view     TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'dbtabview',
      class_interface TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'classintf',
      method          TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'method',
      field           TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'field',
    END OF c_search_type.

  CONSTANTS:
    BEGIN OF c_search_fields,
      object_name_input_key     TYPE string VALUE 'objectName',
      object_name_input_label   TYPE string VALUE 'Object &Name',
      object_filter_input_key   TYPE string VALUE 'objectFilter',
      object_filter_input_label TYPE string VALUE 'Object &Filters',
      method_name_input_key     TYPE string VALUE 'methodName',
      method_name_input_label   TYPE string VALUE '&Method Name',
      method_filter_input_key   TYPE string VALUE 'methodFilter',
      method_filter_input_label TYPE string VALUE 'M&ethod Filter',
    END OF c_search_fields.

  CONSTANTS:
    BEGIN OF c_image_registry_id,
      adt_type TYPE string VALUE 'ADT_OBJECT_TYPE',
    END OF c_image_registry_id.

  CONSTANTS:
    BEGIN OF c_proposal_image_source,
      same_as_filter TYPE string VALUE 'SAME_AS_FILTER',
      proposal       TYPE string VALUE 'PROPOSAL',
    END OF c_proposal_image_source.

  CONSTANTS:
    BEGIN OF c_filter_data_type,
      default TYPE string VALUE '',
      date    TYPE string VALUE 'DATE',
      boolean TYPE string VALUE 'BOOLEAN',
    END OF c_filter_data_type.

  CONSTANTS:
    BEGIN OF c_filter_content_assist_type,
      named_item       TYPE string VALUE 'objectsearch:NamedItemContentAssist',
      fixed_named_item TYPE string VALUE 'objectsearch:FixedValuesContentAssist',
      ris              TYPE string VALUE 'objectsearch:RisContentAssist',
      user             TYPE string VALUE 'objectsearch:UserContentAssist',
    END OF c_filter_content_assist_type.

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
      attribute  TYPE string VALUE 'attr',
      method     TYPE string VALUE 'meth',
      super_type TYPE string VALUE 'super',
      interface  TYPE string VALUE 'intf',
      "! Category of the class/interface (see data element SEOCATEGRY)
      "! --> check against current NW stack to get valid options
      category   TYPE string VALUE 'cat',
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
      "! Holds the name of referenced cds view for behavior classes
      "! (necessary???)
      ref_object TYPE string VALUE 'refObject',
    END OF c_class_intf_search_option.

  CONSTANTS:
    BEGIN OF c_method_search_option,
      param      TYPE string VALUE 'param',
      "! Exposure (for methods/attributes). Possible values are
      "! <ul>
      "!   <li>private</li>
      "!   <li>protected</li>
      "!   <li>public</li>
      "! </ul>
      visibility TYPE string VALUE 'visibility',
      "! <ul>
      "!    <li>instance</li>
      "!    <li>static</li>
      "! </ul>
      level      TYPE string VALUE 'level',
      "! <ul>
      "!    <li>implemented</li>
      "!    <li>redefined</li>
      "!    <li>defined</li>
      "! </ul>
      status     TYPE string VALUE 'status',
      flag       TYPE string VALUE 'flag',
    END OF c_method_search_option.

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
    BEGIN OF c_method_types,
      general            TYPE string VALUE 'GENERAL',
      constructor        TYPE string VALUE 'CONSTRUCTOR',
      event_handler      TYPE string VALUE 'EVENT_HANDLER',
      virtual_getter     TYPE string VALUE 'VIRTUAL_GETTER',
      virtual_setter     TYPE string VALUE 'VIRTUAL_SETTER',
      test               TYPE string VALUE 'TEST',
      cds_table_function TYPE string VALUE 'CDS_TABLE_FUNCTION',
      amdp_ddl_object    TYPE string VALUE 'AMDP_DDL_OBJECT',
    END OF c_method_types.

  CONSTANTS:
    BEGIN OF c_visibility,
      private   TYPE string VALUE 'PRIVATE',
      protected TYPE string VALUE 'PROTECTED',
      public    TYPE string VALUE 'PUBLIC',
    END OF c_visibility.

  CONSTANTS:
    "! External method status for ADT UI
    BEGIN OF c_method_status,
      standard    TYPE string VALUE 'STANDARD',
      implemented TYPE string VALUE 'IMPLEMENTED',
      redefined   TYPE string VALUE 'REDEFINED',
    END OF c_method_status.

  CONSTANTS:
    "! Internal method status
    BEGIN OF c_method_status_int,
      standard    TYPE c LENGTH 1 VALUE '1',
      implemented TYPE c LENGTH 1 VALUE '2',
      redefined   TYPE c LENGTH 1 VALUE '3',
    END OF c_method_status_int.

  CONSTANTS:
    BEGIN OF c_method_flags,
      optional         TYPE string VALUE 'OPTIONAL',
      abstract         TYPE string VALUE 'ABSTRACT',
      final            TYPE string VALUE 'FINAL',
      class_exceptions TYPE string VALUE 'CLASS_EXCEPTIONS',
    END OF c_method_flags.

  CONSTANTS:
    BEGIN OF c_method_level,
      instance TYPE string VALUE 'INSTANCE',
      static   TYPE string VALUE 'STATIC',
    END OF c_method_level.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">General search options</p>
    BEGIN OF c_general_search_params,
      object_name            TYPE string VALUE 'objectName',
      object_type            TYPE string VALUE 'objectType',
      max_rows               TYPE string VALUE 'maxrows',
      user                   TYPE string VALUE 'owner',
      changed_by             TYPE string VALUE 'changedby',
      application_component  TYPE string VALUE 'appl',
      release_state          TYPE string VALUE 'api',
      description            TYPE string VALUE 'desc',
      type                   TYPE string VALUE 'type',
      package                TYPE string VALUE 'package',
      created_on             TYPE string VALUE 'created',
      changed_on             TYPE string VALUE 'changed',
      use_and_for_filters    TYPE string VALUE 'useAndForFilters',
      read_api_state         TYPE string VALUE 'withApiState',
      get_all_results        TYPE string VALUE 'getAllResults',
      read_package_hierarchy TYPE string VALUE 'withPackageHierarchy',
    END OF c_general_search_params.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options for CDS Search</p>
    BEGIN OF c_cds_search_params,
      field             TYPE string VALUE 'field',
      select_from       TYPE string VALUE 'from',
      association       TYPE string VALUE 'assoc',
      only_local_assocs TYPE string VALUE 'localDeclaredAssocOnly',
      annotation        TYPE string VALUE 'anno',
      param             TYPE string VALUE 'param',
      params            TYPE string VALUE 'params',
      extended_by       TYPE string VALUE 'extby',
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
      field          TYPE string VALUE 'field',
      delivery_class TYPE string VALUE 'dclass',
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

  CONSTANTS:
    "! Constants for value help providers for search parameters in
    "! search input fields
    BEGIN OF c_content_assist,
      category_scheme TYPE string VALUE 'http://www.devepos.com/adt/saat/v2/objectsearch',
      BEGIN OF terms,
        cds_field         TYPE string VALUE 'cdsfield',
        appl_comp         TYPE string VALUE 'applcomp',
        table_field       TYPE string VALUE 'tablefield',
        annotation        TYPE string VALUE 'annotation',
        annotatio_value   TYPE string VALUE 'annotationvalue',
        db_entity         TYPE string VALUE 'dbentity',
        release_state     TYPE string VALUE 'releasestate',
        cds_type          TYPE string VALUE 'cdstype',
        cds_extension     TYPE string VALUE 'cdsextension',
        table_type        TYPE string VALUE 'tabletype',
        abap_language     TYPE string VALUE 'abaplanguage',
        class_category    TYPE string VALUE 'classcategory',
        class_flag        TYPE string VALUE 'classflag',
        class_type        TYPE string VALUE 'classtype',
        table_deliv_class TYPE string VALUE 'tabledeliveryclass',
      END OF terms,
    END OF c_content_assist.
ENDINTERFACE.
