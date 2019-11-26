"! <p class="shorttext synchronized" lang="en">Configuration for Object Search Implementation</p>
INTERFACE zif_sat_object_search_config
  PUBLIC .

  TYPES:
    ty_t_options TYPE RANGE OF string,
    "! <p class="shorttext synchronized" lang="en">Setting for search option</p>
    BEGIN OF ty_s_option_setting,
      option         TYPE string,
      allowed_length TYPE i,
      single         TYPE abap_bool,
      key_value      TYPE abap_bool,
      no_negation    TYPE abap_bool,
    END OF ty_s_option_setting,
    "! <p class="shorttext synchronized" lang="en">List of option configurations</p>
    ty_t_option_setting TYPE STANDARD TABLE OF ty_s_option_setting WITH KEY option.

  "! <p class="shorttext synchronized" lang="en">Returns a table of allowed search options</p>
  "!
  "! @parameter rt_options | <p class="shorttext synchronized" lang="en">Range of allowed options</p>
  METHODS get_allowed_options
    RETURNING
      VALUE(rt_options) TYPE ty_t_options.

  "! <p class="shorttext synchronized" lang="en">Retrieves configuration for search option</p>
  "!
  "! @parameter iv_option | <p class="shorttext synchronized" lang="en">The name of a search option</p>
  "! @parameter rs_option | <p class="shorttext synchronized" lang="en">The configuration of the option</p>
  METHODS get_option_config
    IMPORTING
      iv_option        TYPE string
    RETURNING
      VALUE(rs_option) TYPE ty_s_option_setting.

  "! <p class="shorttext synchronized" lang="en">Checks if an option exists in this configuration</p>
  "!
  "! @parameter iv_option | <p class="shorttext synchronized" lang="en">The name of a search option</p>
  "! @parameter rf_has_option | <p class="shorttext synchronized" lang="en">'X' if the option exists</p>
  METHODS has_option
    IMPORTING
      iv_option            TYPE string
    RETURNING
      VALUE(rf_has_option) TYPE abap_bool.

  "! <p class="shorttext synchronized" lang="en">Returns the search type</p>
  "!
  "! @parameter rv_type | <p class="shorttext synchronized" lang="en">The search type</p>
  METHODS get_type
    RETURNING
      VALUE(rv_type) TYPE zif_sat_ty_object_search=>ty_search_type.

  "! <p class="shorttext synchronized" lang="en">Maps search option to another</p>
  "!
  "! @parameter cv_option | <p class="shorttext synchronized" lang="en">The option name to be mapped</p>
  METHODS map_option DEFAULT IGNORE
    CHANGING
      cv_option TYPE string.

  "! <p class="shorttext synchronized" lang="en">Returns the search options with their settings</p>
  "!
  "! @parameter rt_options | <p class="shorttext synchronized" lang="en">List of of search option configurations</p>
  METHODS get_options
    RETURNING
      VALUE(rt_options) TYPE ty_t_option_setting.
ENDINTERFACE.
