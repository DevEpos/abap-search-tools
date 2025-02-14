"! <p class="shorttext synchronized">General options for TABL like object search</p>
INTERFACE zif_sat_c_os_tabl_options
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options TABL like search</p>
    BEGIN OF c_filter_key,
      include_usage        TYPE string VALUE 'include',
      field                TYPE string VALUE 'field',
      enhancement_category TYPE string VALUE 'enhcat',
    END OF c_filter_key.

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
