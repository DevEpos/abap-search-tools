"! <p class="shorttext synchronized">Options/Constants for View Search</p>
INTERFACE zif_sat_c_os_view_options
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options View search</p>
    BEGIN OF c_filter_key,
      field          TYPE string VALUE 'field',
      delivery_class TYPE string VALUE 'dlvclass',
      flag           TYPE string VALUE 'flag',
      primary_table  TYPE string VALUE 'primtab',
      base_table     TYPE string VALUE 'basetab',
    END OF c_filter_key.

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
ENDINTERFACE.
