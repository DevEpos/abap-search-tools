"! <p class="shorttext synchronized">Options/Constants for CDS Search</p>
INTERFACE zif_sat_c_os_cds_options
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options for CDS Search</p>
    BEGIN OF c_filter_key,
      field       TYPE string VALUE 'field',
      select_from TYPE string VALUE 'from',
      association TYPE string VALUE 'assoc',
      annotation  TYPE string VALUE 'anno',
      param       TYPE string VALUE 'param',
      params      TYPE string VALUE 'params',
      extended_by TYPE string VALUE 'extby',
    END OF c_filter_key.

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
      projection      TYPE string VALUE 'PROJECTION',
    END OF c_type_option_value.
ENDINTERFACE.
