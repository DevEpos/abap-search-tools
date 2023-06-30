"! <p class="shorttext synchronized">Configuration for Object Search Implementation</p>
INTERFACE zif_sat_object_search_config
  PUBLIC.

  "! <p class="shorttext synchronized">Retrieves configuration of search type</p>
  METHODS get_search_config DEFAULT IGNORE
    RETURNING
      VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_search_type.

  "! <p class="shorttext synchronized">Returns a table of allowed search options</p>
  "!
  "! @parameter rt_options | Range of allowed options
  METHODS get_allowed_options
    RETURNING
      VALUE(rt_options) TYPE zif_sat_ty_object_search=>ty_t_options.

  "! <p class="shorttext synchronized">Retrieves configuration for search option</p>
  "!
  "! @parameter iv_option | The name of a search option
  "! @parameter iv_target | The target of the option (i.e. for options of sub types like e.g. method)
  "! @parameter rs_option | The configuration of the option
  METHODS get_option_config
    IMPORTING
      iv_option        TYPE string
      iv_target        TYPE string OPTIONAL
    RETURNING
      VALUE(rs_option) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

  "! <p class="shorttext synchronized">Checks if an option exists in this configuration</p>
  "!
  "! @parameter iv_option     | The name of a search option
  "! @parameter iv_target     | Target of the search option
  "! @parameter rf_has_option | 'X' if the option exists
  METHODS has_option
    IMPORTING
      iv_option            TYPE string
      iv_target            TYPE string OPTIONAL
    RETURNING
      VALUE(rf_has_option) TYPE abap_bool.

  "! <p class="shorttext synchronized">Returns the search type</p>
  "!
  "! @parameter rv_type | The search type
  METHODS get_type
    RETURNING
      VALUE(rv_type) TYPE zif_sat_ty_object_search=>ty_search_type.

  "! <p class="shorttext synchronized">Maps search option to another</p>
  "!
  "! @parameter cv_option | The option name to be mapped
  METHODS map_option DEFAULT IGNORE
    CHANGING
      cv_option TYPE string.

  "! <p class="shorttext synchronized">Returns the search options with their settings</p>
  "!
  "! @parameter rt_options | List of of search option configurations
  METHODS get_options
    RETURNING
      VALUE(rt_options) TYPE zif_sat_ty_object_search=>ty_t_query_filter.
ENDINTERFACE.
