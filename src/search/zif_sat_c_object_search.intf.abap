"! <p class="shorttext synchronized">Constants for Object search</p>
INTERFACE zif_sat_c_object_search
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_search_type,
      cds_view        TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'cds',
      db_tab          TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'dbtab',
      ddic_view       TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'ddicview',
      class_interface TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'classintf',
      method          TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'method',
      db_field        TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'dbfield',
      cds_field       TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'cdsfield',
      message         TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'message',
    END OF c_search_type.

  CONSTANTS:
    BEGIN OF c_search_fields,
      object_name_input_key      TYPE string VALUE 'objectName',
      object_name_input_label    TYPE string VALUE 'Object &Name',
      object_filter_input_key    TYPE string VALUE 'objectFilter',
      object_filter_input_label  TYPE string VALUE 'Object &Filters',
      method_name_input_key      TYPE string VALUE 'methodName',
      method_name_input_label    TYPE string VALUE '&Method Name',
      method_filter_input_key    TYPE string VALUE 'methodFilter',
      method_filter_input_label  TYPE string VALUE 'M&ethod Filter',
      message_text_input_key     TYPE string VALUE 'messageText',
      message_text_input_label   TYPE string VALUE '&Message Short Text',
      message_filter_input_key   TYPE string VALUE 'messageFilter',
      message_filter_input_label TYPE string VALUE 'M&essage Filter',
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

  CONSTANTS c_image_id_prefix TYPE string VALUE 'imageId='.

  CONSTANTS:
    BEGIN OF c_filter_data_type,
      default TYPE string VALUE '',
      date    TYPE string VALUE 'DATE',
      boolean TYPE string VALUE 'BOOLEAN',
    END OF c_filter_data_type.

  CONSTANTS:
    BEGIN OF c_custom_option_data_type,
      string  TYPE string VALUE 'STRING',
      boolean TYPE string VALUE 'BOOLEAN',
    END OF c_custom_option_data_type.

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
      exception  TYPE string VALUE 'exc',
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
      general        TYPE string VALUE 'GENERAL',
      constructor    TYPE string VALUE 'CONSTRUCTOR',
      event_handler  TYPE string VALUE 'EVENT_HANDLER',
      virtual_getter TYPE string VALUE 'VIRTUAL_GETTER',
      virtual_setter TYPE string VALUE 'VIRTUAL_SETTER',
      test           TYPE string VALUE 'TEST',
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
    BEGIN OF c_custom_options,
      BEGIN OF method,
        target_incl_for_admin_data TYPE string VALUE 'targetIncludesForAdminData',
      END OF method,
    END OF c_custom_options.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">General search options</p>
    BEGIN OF c_general_search_params,
      object_name            TYPE string VALUE 'objectName',
      object_type            TYPE string VALUE 'objectType',
      max_rows               TYPE string VALUE 'maxrows',
      user                   TYPE string VALUE 'owner',
      changed_by             TYPE string VALUE 'changedby',
      application_component  TYPE string VALUE 'appl',
      software_component     TYPE string VALUE 'comp',
      description            TYPE string VALUE 'desc',
      type                   TYPE string VALUE 'type',
      package                TYPE string VALUE 'package',
      created_on             TYPE string VALUE 'created',
      changed_on             TYPE string VALUE 'changed',
      maintenance            TYPE string VALUE 'maintflag',
      use_and_for_filters    TYPE string VALUE 'useAndForFilters',
      get_all_results        TYPE string VALUE 'getAllResults',
      read_package_hierarchy TYPE string VALUE 'withPackageHierarchy',
    END OF c_general_search_params.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options for CDS Search</p>
    BEGIN OF c_cds_search_params,
      field       TYPE string VALUE 'field',
      select_from TYPE string VALUE 'from',
      association TYPE string VALUE 'assoc',
      annotation  TYPE string VALUE 'anno',
      param       TYPE string VALUE 'param',
      params      TYPE string VALUE 'params',
      extended_by TYPE string VALUE 'extby',
    END OF c_cds_search_params.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Types for class/interface</p>
    BEGIN OF c_class_types,
      class     TYPE string VALUE 'CLAS',
      interface TYPE string VALUE 'INTF',
    END OF c_class_types.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options DB Table search</p>
    BEGIN OF c_dbtab_search_params,
      field                TYPE string VALUE 'field',
      delivery_class       TYPE string VALUE 'dlvclass',
      flag                 TYPE string VALUE 'flag',
      size_category        TYPE string VALUE 'sizecat',
      buffering            TYPE string VALUE 'buffering',
      buffering_type       TYPE string VALUE 'buffertype',
      data_class           TYPE string VALUE 'dataclass',
      enhancement_category TYPE string VALUE 'enhcat',
      storage_type         TYPE string VALUE 'storetype',
      include_usage        TYPE string VALUE 'include',
    END OF c_dbtab_search_params.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options View search</p>
    BEGIN OF c_ddicview_search_params,
      field          TYPE string VALUE 'field',
      delivery_class TYPE string VALUE 'dlvclass',
      flag           TYPE string VALUE 'flag',
      primary_table  TYPE string VALUE 'primtab',
      base_table     TYPE string VALUE 'basetab',
    END OF c_ddicview_search_params.

  CONSTANTS:
    BEGIN OF c_message_search_params,
      self_explanatory TYPE string VALUE 'selfexpl',
    END OF c_message_search_params.

  CONSTANTS:
    BEGIN OF c_tab_enh_categories,
      BEGIN OF int,
        not_classified        TYPE dd02l-exclass VALUE '0',
        not_extendable        TYPE dd02l-exclass VALUE '1',
        char_like             TYPE dd02l-exclass VALUE '2',
        char_like_and_numeric TYPE dd02l-exclass VALUE '3',
        any                   TYPE dd02l-exclass VALUE '4',
      END OF int,
      BEGIN OF ext,
        not_classified        TYPE string VALUE 'NOT_CLASSIFIED',
        not_extendable        TYPE string VALUE 'NOT_EXTENDABLE',
        char_like             TYPE string VALUE 'CHARACTER_LIKE',
        char_like_and_numeric TYPE string VALUE 'CHARACTER_LIKE_AND_NUMERIC',
        any                   TYPE string VALUE 'ANY',
      END OF ext,
    END OF c_tab_enh_categories.

  CONSTANTS:
    BEGIN OF c_db_flags,
      client_dep        TYPE string VALUE 'CLIENT_DEP',
      used_in_shlp      TYPE string VALUE 'USED_IN_SHLP',
      change_log_active TYPE string VALUE 'CHANGE_LOG_ACTIVE',
    END OF c_db_flags.

  CONSTANTS:
    BEGIN OF c_db_buffer_status,
      BEGIN OF int,
        off             TYPE dd09l-bufallow VALUE 'N',
        allowed_but_off TYPE dd09l-bufallow VALUE 'A',
        on              TYPE dd09l-bufallow VALUE 'X',
      END OF int,
      BEGIN OF ext,
        off             TYPE string VALUE 'OFF',
        allowed_but_off TYPE string VALUE 'ALLOWED_BUT_OFF',
        on              TYPE string VALUE 'ON',
      END OF ext,
    END OF c_db_buffer_status.

  CONSTANTS:
    BEGIN OF c_table_maintenance,
      BEGIN OF int,
        allowed_with_restr TYPE maintflag VALUE '',
        not_allowed        TYPE maintflag VALUE 'N',
        allowed            TYPE maintflag VALUE 'X',
      END OF int,
      BEGIN OF ext,
        allowed_with_restr TYPE string VALUE 'ALLOWED_WITH_RESTR',
        not_allowed        TYPE string VALUE 'NOT_ALLOWED',
        allowed            TYPE string VALUE 'ALLOWED',
      END OF ext,
    END OF c_table_maintenance.

  CONSTANTS:
    BEGIN OF c_db_buffer_type,
      BEGIN OF int,
        no_buffering      TYPE dd09l-pufferung VALUE '',
        single_entries    TYPE dd09l-pufferung VALUE 'P',
        full_with_gen_key TYPE dd09l-pufferung VALUE 'G',
        full_table        TYPE dd09l-pufferung VALUE 'X',
      END OF int,
      BEGIN OF ext,
        no_buffering      TYPE string VALUE 'NO_BUFFERING',
        single_entries    TYPE string VALUE 'SINGLE_ENTRIES',
        full_with_gen_key TYPE string VALUE 'FULL_WITH_GEN_KEY',
        full_table        TYPE string VALUE 'FULL_TABLE',
      END OF ext,
    END OF c_db_buffer_type.

  CONSTANTS:
    BEGIN OF c_view_class,
      BEGIN OF int,
        database    TYPE viewclass VALUE 'D',
        help        TYPE viewclass VALUE 'H',
        projection  TYPE viewclass VALUE 'P',
        maintenance TYPE viewclass VALUE 'C',
      END OF int,
      BEGIN OF ext,
        database    TYPE string VALUE 'DATABASE',
        help        TYPE string VALUE 'HELP',
        projection  TYPE string VALUE 'PROJECTION',
        maintenance TYPE string VALUE 'MAINTENANCE',
      END OF ext,
    END OF c_view_class.

  CONSTANTS:
    BEGIN OF c_table_storage_type,
      BEGIN OF int,
        column    TYPE ddroworcolst VALUE 'C',
        row       TYPE ddroworcolst VALUE 'R',
        undefined TYPE ddroworcolst VALUE space,
      END OF int,
      BEGIN OF ext,
        column    TYPE string VALUE 'COLUMN',
        row       TYPE string VALUE 'ROW',
        undefined TYPE string VALUE 'UNDEFINED',
      END OF ext,
    END OF c_table_storage_type.

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
        software_comp     TYPE string VALUE 'softcomp',
        table_field       TYPE string VALUE 'tablefield',
        annotation        TYPE string VALUE 'annotation',
        annotatio_value   TYPE string VALUE 'annotationvalue',
        db_entity         TYPE string VALUE 'dbentity',
        cds_type          TYPE string VALUE 'cdstype',
        cds_extension     TYPE string VALUE 'cdsextension',
        abap_language     TYPE string VALUE 'abaplanguage',
        class_category    TYPE string VALUE 'classcategory',
        class_flag        TYPE string VALUE 'classflag',
        class_type        TYPE string VALUE 'classtype',
        table_deliv_class TYPE string VALUE 'tabledeliveryclass',
        table_data_class  TYPE string VALUE 'tabledataclass',
        view_field        TYPE string VALUE 'viewfield',
        view_root_tab     TYPE string VALUE 'viewroottable',
        view_base_tab     TYPE string VALUE 'viewbasetable',
        db_tab_include    TYPE string VALUE 'dbtableinclude',
      END OF terms,
    END OF c_content_assist.
ENDINTERFACE.
