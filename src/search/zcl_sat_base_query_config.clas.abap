"! <p class="shorttext synchronized" lang="en">Base configuration for Object Search Query</p>
CLASS zcl_sat_base_query_config DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_config
      ABSTRACT METHODS get_type.
    INTERFACES zif_sat_c_object_search.
  PROTECTED SECTION.
    ALIASES:
      c_search_option FOR zif_sat_c_object_search~c_search_option,
      c_general_options FOR zif_sat_c_object_search~c_general_search_params.
    DATA mt_options TYPE zif_sat_object_search_config=>ty_t_option_setting.
    DATA mt_allowed_options TYPE zif_sat_object_search_config=>ty_t_options.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_base_query_config IMPLEMENTATION.

  METHOD zif_sat_object_search_config~get_allowed_options.
    IF mt_allowed_options IS INITIAL.
      mt_allowed_options = VALUE #(
        FOR option IN mt_options ( sign = 'I' option = 'EQ' low = option-option )
      ).
    ENDIF.
    rt_options = mt_allowed_options.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_option_config.
    rs_option = mt_options[ option = iv_option ].
  ENDMETHOD.

  METHOD zif_sat_object_search_config~has_option.
    rf_has_option = xsdbool( line_exists( mt_options[ option = iv_option ] ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_options.
    rt_options = mt_options.
  ENDMETHOD.

ENDCLASS.
