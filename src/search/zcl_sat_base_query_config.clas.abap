"! <p class="shorttext synchronized">Base configuration for Object Search Query</p>
CLASS zcl_sat_base_query_config DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_config
      ABSTRACT METHODS get_type.
    INTERFACES zif_sat_c_object_search.

  PROTECTED SECTION.
    CONSTANTS c_object_name_input_key     TYPE string VALUE 'objectName'.
    CONSTANTS c_object_name_input_label   TYPE string VALUE 'Object Name'.
    CONSTANTS c_object_filter_input_key   TYPE string VALUE 'objectFilter'.
    CONSTANTS c_object_filter_input_label TYPE string VALUE 'Object Filters'.

    ALIASES c_search_option   FOR zif_sat_c_object_search~c_search_option.
    ALIASES c_general_options FOR zif_sat_c_object_search~c_general_search_params.

    DATA mt_options         TYPE zif_sat_ty_object_search=>ty_t_query_filter.
    DATA mt_allowed_options TYPE zif_sat_ty_object_search=>ty_t_options.
    DATA ms_search_type     TYPE zif_sat_ty_object_search=>ty_s_search_type.

    METHODS get_user_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_package_Filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_description_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_max_rows_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_rel_state_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_base_query_config IMPLEMENTATION.
  METHOD zif_sat_object_search_config~get_allowed_options.
    IF mt_allowed_options IS INITIAL.
      mt_allowed_options = VALUE #( FOR option IN mt_options
                                    ( sign = 'I' option = 'EQ' low = option-name ) ).
    ENDIF.
    rt_options = mt_allowed_options.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_option_config.
    rs_option = mt_options[ name = iv_option ].
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_search_config.
    result = ms_search_type.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~has_option.
    rf_has_option = xsdbool( line_exists( mt_options[ name = iv_option ] ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_options.
    rt_options = mt_options.
  ENDMETHOD.

  METHOD get_description_filt_conf.
    result = VALUE #( name = c_general_options-description allowed_length = 40 patterns = abap_true no_uppercase = abap_true ).
  ENDMETHOD.

  METHOD get_max_rows_filt_conf.
    result = VALUE #( name = c_general_options-max_rows single = abap_true no_negation = abap_true internal = abap_true ).
  ENDMETHOD.

  METHOD get_package_filt_conf.
    result = VALUE #(
        name           = c_general_options-package
        allowed_length = 30
        patterns       = abap_true
        content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-ris
                                  adt_object_type = zif_sat_c_tadir_types=>package ) ).
  ENDMETHOD.

  METHOD get_user_filt_conf.
    result = VALUE #(
        name           = c_general_options-user
        patterns       = abap_true
        allowed_length = 12
        content_assist = VALUE #( assist_type = zif_sat_c_object_search=>c_filter_content_assist_type-user ) ).
  ENDMETHOD.

  METHOD get_rel_state_filt_conf.
    result = VALUE #(
        name           = c_general_options-release_state
        caching        = abap_true
        content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                  category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                  category_term   = zif_sat_c_object_search=>c_content_assist-terms-release_state ) ).
  ENDMETHOD.
ENDCLASS.
