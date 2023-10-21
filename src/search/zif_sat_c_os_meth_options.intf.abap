"! <p class="shorttext synchronized">Options/Constants for Method Search</p>
INTERFACE zif_sat_c_os_meth_options
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_search_fields,
      method_name_input_key     TYPE string VALUE 'methodName',
      method_name_input_label   TYPE string VALUE '&Method Name',
      method_filter_input_key   TYPE string VALUE 'methodFilter',
      method_filter_input_label TYPE string VALUE 'M&ethod Filter',
    END OF c_search_fields.

  CONSTANTS:
    BEGIN OF c_filter_key,
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
    END OF c_filter_key.

  CONSTANTS:
    BEGIN OF c_method_types,
      general            TYPE string VALUE 'GENERAL',
      constructor        TYPE string VALUE 'CONSTRUCTOR',
      event_handler      TYPE string VALUE 'EVENT_HANDLER',
      virtual_getter     TYPE string VALUE 'VIRTUAL_GETTER',
      virtual_setter     TYPE string VALUE 'VIRTUAL_SETTER',
      test               TYPE string VALUE 'TEST',
      cds_table_function TYPE string VALUE 'CDS_TABLE_FUNCTION',
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
ENDINTERFACE.
