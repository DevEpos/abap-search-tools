"! <p class="shorttext synchronized">Options/Constants for DB Tab Search</p>
INTERFACE zif_sat_c_os_dtab_options
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options DB Table search</p>
    BEGIN OF c_filter_key,
      delivery_class TYPE string VALUE 'dlvclass',
      flag           TYPE string VALUE 'flag',
      size_category  TYPE string VALUE 'sizecat',
      buffering      TYPE string VALUE 'buffering',
      buffering_type TYPE string VALUE 'buffertype',
      data_class     TYPE string VALUE 'dataclass',
      storage_type   TYPE string VALUE 'storetype',
    END OF c_filter_key.

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
    BEGIN OF c_db_flags,
      client_dep        TYPE string VALUE 'CLIENT_DEP',
      used_in_shlp      TYPE string VALUE 'USED_IN_SHLP',
      is_gtt            TYPE string VALUE 'IS_GTT',
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
ENDINTERFACE.
