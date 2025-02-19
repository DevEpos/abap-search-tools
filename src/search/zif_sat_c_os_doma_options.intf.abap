"! <p class="shorttext synchronized">Search options for data element</p>
INTERFACE zif_sat_c_os_doma_options
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_filter_key,
      length      TYPE string VALUE 'length',
      outlength   TYPE string VALUE 'outlength',
      decimals    TYPE string VALUE 'decimals',
      value_table TYPE string VALUE 'valuetab',
      data_type   TYPE string VALUE 'dtype',
      flag        TYPE string VALUE 'flag',
      conv_exit   TYPE string VALUE 'convexit',
      fix_value   TYPE string VALUE 'fixval',
    END OF c_filter_key.

  CONSTANTS:
    BEGIN OF c_flag,
      lowercase  TYPE string VALUE 'LOWERCASE',
      fix_values TYPE string VALUE 'FIX_VALUES',
    END OF c_flag.

  CONSTANTS:
    BEGIN OF c_type,
      domain        TYPE zsat_i_domain-type VALUE 'DOMAIN',
      append_domain TYPE zsat_i_domain-type VALUE 'APPEND_DOMAIN',
    END OF c_type.
ENDINTERFACE.
