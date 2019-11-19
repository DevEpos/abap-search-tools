"! <p class="shorttext synchronized" lang="en">Constants for Object search</p>
INTERFACE zif_sat_c_object_search
  PUBLIC .

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search option for object browser</p>
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
