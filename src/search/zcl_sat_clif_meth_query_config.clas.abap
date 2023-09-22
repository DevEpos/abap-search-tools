"! <p class="shorttext synchronized">Query Config for Method search</p>
CLASS zcl_sat_clif_meth_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_clsintf_query_config
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type          REDEFINITION.
    METHODS zif_sat_object_search_config~get_option_config REDEFINITION.
    METHODS zif_sat_object_search_config~get_output_config REDEFINITION.
    METHODS zif_sat_object_search_config~has_option        REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_method_options FOR zif_sat_c_object_search~c_method_search_option.

    CONSTANTS:
      BEGIN OF c_image_keys,
        visibility TYPE string VALUE 'ABAP:IMG_VISIBILITY',
        level      TYPE string VALUE 'ABAP:IMG_METHOD_LEVEL',
        status     TYPE string VALUE 'ABAP:IMG_METHOD_STATUS',
        exception  TYPE string VALUE 'ABAP:IMG_METH_EXCEPTION',
      END OF c_image_keys.

    DATA mt_method_options TYPE zif_sat_ty_object_search=>ty_query_filters.
    DATA mv_option_prefix_pattern TYPE string.

    METHODS delete_invalid_obj_filters.

    METHODS get_flag_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_param_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_exception_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_visibility_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_status_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_level_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_clif_meth_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-method.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_option_config.
    IF iv_target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.
      rs_option = mt_method_options[ name = iv_option ].
    ELSE.
      rs_option = super->zif_sat_object_search_config~get_option_config( iv_option ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_output_config.
    result = VALUE #( is_list_output_supported = abap_true
                      types_for_list           = VALUE #( ( zif_sat_c_object_types=>class_method_impl )
                                                          ( zif_sat_c_object_types=>interface_method ) )
                      groupings                = VALUE #( ( c_output_grouping_level-package ) ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~has_option.
    IF iv_target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.
      rf_has_option = xsdbool( line_exists( mt_method_options[ name = iv_option ] ) ).
    ELSE.
      rf_has_option = super->zif_sat_object_search_config~has_option( iv_option = iv_option iv_target = iv_target ).
    ENDIF.
  ENDMETHOD.

  METHOD build_config.
    super->build_config( ).

    delete_invalid_obj_filters( ).

    mt_method_options = VALUE zif_sat_ty_object_search=>ty_query_filters( ( get_user_filt_conf( ) )
                                                                          ( get_created_on_filt_conf( ) )
                                                                          ( get_changed_by_filt_conf( ) )
                                                                          ( get_changed_on_filt_conf( ) )
                                                                          ( get_description_filt_conf( ) )
                                                                          ( get_type_filter( ) )
                                                                          ( get_flag_filter( ) )
                                                                          ( get_param_filter( ) )
                                                                          ( get_exception_filter( ) )
                                                                          ( get_level_filter( ) )
                                                                          ( get_status_filter( ) )
                                                                          ( get_visibility_filter( ) ) ).

    ms_search_type-label          = 'Method'.
    ms_search_type-img_info       = VALUE #( img_key     = c_clif_image_keys-method
                                             img_encoded = get_clif_image( c_clif_image_keys-method ) ).
    ms_search_type-name           = zif_sat_c_object_search=>c_search_type-method.
    ms_search_type-inputs         = VALUE #(
        BASE ms_search_type-inputs
        ( name    = zif_sat_c_object_search=>c_search_fields-method_name_input_key
          label   = zif_sat_c_object_search=>c_search_fields-method_name_input_label )
        ( name    = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
          label   = zif_sat_c_object_search=>c_search_fields-method_filter_input_label
          filters = mt_method_options ) ).

    ms_search_type-custom_options = VALUE #(
        BASE ms_search_type-custom_options
        ( key         = zif_sat_c_object_search=>c_custom_options-method-target_incl_for_admin_data
          type        = zif_sat_c_object_search=>c_custom_option_data_type-boolean
          label       = 'Set Focus of Admin-Data Filters to Method &Include'
          description = |If admin data filters like 'changed' or 'changedby' are used the\n| &&
                        |SELECT Target will be switched to the Method-Include instead of the Method-Definition|  ) ).
  ENDMETHOD.

  METHOD delete_invalid_obj_filters.
    DELETE mt_options WHERE    name = c_class_intf_search_option-attribute
                            OR name = c_class_intf_search_option-method.

    DATA(lr_object_filters) = REF #( ms_search_type-inputs[
                                         name = zif_sat_c_object_search=>c_search_fields-object_filter_input_key ]-filters ).

    DELETE lr_object_filters->* WHERE    name = c_class_intf_search_option-attribute
                                      OR name = c_class_intf_search_option-method.
  ENDMETHOD.

  METHOD get_flag_filter.
    result = VALUE #(
        name             = c_method_options-flag
        description      = 'Flag'
        long_description = |Use '{ c_method_options-flag }' to restrict the result to methods with specific features.\n\n| &&
                           |Exmaple:\n   { c_method_options-flag } : abstract|
        img_info         = VALUE #( img_key     = c_general_image_keys-checked_box
                                    img_encoded = get_general_image( c_general_image_keys-checked_box ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_values = VALUE #(
                ( name = zif_sat_c_object_search=>c_method_flags-optional         description = 'Implementation is Optional' )
                ( name = zif_sat_c_object_search=>c_method_flags-abstract         description = 'Abstract' )
                ( name = zif_sat_c_object_search=>c_method_flags-final            description = 'Final' )
                ( name = zif_sat_c_object_search=>c_method_flags-class_exceptions description = 'Class Based Exceptions are used' ) ) ) ).
  ENDMETHOD.

  METHOD get_exception_filter.
    result = VALUE #(
        name             = c_method_options-exception
        description      = 'Exception'
        long_description = |Use '{ c_method_options-exception }' to restrict the result to methods with specific exceptions.\n\n| &&
                           |Example:\n   { c_method_options-exception } : cx_static_check|
        img_info         = VALUE #(
            img_key     = c_image_keys-exception
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA90lEQVR4nGNgGLTg4DyN+IPzdT4dmK/188AC/TqSDdg7S/3/j4et/3896fy/b5bmb5IN2D1D/f/f553//7/s+b97htp/kg3YOU3t/7d7` &&
                          `Nf9/PKj/D2KTbMCuaRrv3l3J/f/+at5/EJt0A2ZoXXl6Iu7/y7PJ/3dN17xEsgE7p2rsu7c/+D8Ib5+ktpZkA7ZPUZ99Y4fX/6tb3H5t7lNqJ9mATV1KPlsnqH8F4i8bu5Q9SDZgf4MDy/pOhYKNnYo5` &&
                          `+xsUOMhwgfKVg3P0P4Hwxm6l6yQbsKZV/v+3a1FgDGKTbkCLQt7qZvm/q5vl/q9uk68i2QBiAQCtj4Cw8dSVmwAAAABJRU5ErkJggg==` )
        patterns         = abap_true ).
  ENDMETHOD.

  METHOD get_param_filter.
    result = VALUE #(
        name             = c_method_options-param
        description      = 'Parameter'
        long_description = |Use '{ c_method_options-param }' to restrict the result to methods with specific parameters.\n\n| &&
                           |Example:\n   { c_method_options-param } : iv_user_name|
        img_info         = VALUE #( img_key     = c_general_image_keys-param
                                    img_encoded = get_general_image( c_general_image_keys-param ) )
        patterns         = abap_true ).
  ENDMETHOD.

  METHOD get_visibility_filter.
    result = VALUE #(
        name             = c_method_options-visibility
        description      = 'Visibility'
        long_description = |Use '{ c_method_options-visibility }' to limit query result to methods with a certain exposure.\n\n| &&
                           |Example:\n   { c_method_options-visibility } : public|
        img_info         = VALUE #(
            img_key     = c_image_keys-visibility
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAI6SURBVDhPpZJLaBNRFIb/mTuPPGisIC0SkyYp` &&
                          `tVoflIJFiwiNVGnRLnUjiAvb0pW40l26LG7EhQtBRBGJ6EYEjVJThZYKuvGxUFPaCiXYaBv7yDwyc+91kg5RqjGCH/wM98z5f7jnXAH/gJzoa/fI4mXOeTvlUGQivtdNOmyNPHpdM6Bklggm+2NNUk8o` &&
                          `pBBBwPh8FvemZ3TbZnHi9lXF37Pj/rFoOHQ8ElEkUUQpoHlTACohZHpluUN0+6rCGN0T86hKJreAXxVWiWgx1lbzCr2XDmkdAZ93BlvcyjqUczzILel/vcKz661DXQGhP6YYQtHU8NFUYNh2WRnNwDKl` &&
                          `76oGjF1rO0+IOtp55KISbo2Dzabhl0Xk/Y3IWhQf1nSd2jj5x4DU1ZYhIqqj+/sueH1eGZJoIxjtgvbpCeaWFu2xNfmNs4ET1sjjl+UZvNp7gJedDmbMgHHYRufRQdT5nHxWXP8hyDCpgsnUFb1oaAO9` &&
                          `56Zvl8qVLewaPo3dg6cQjB8Ec2IlagCFr45y69JyEMxVcMZBqWtyqAQIzmBYNouGhQC25qOYeHoTepGAO82cMpi2iomxGwVTM5L+1W1J1/YzgH1bBfvyvazIXAiNK9sxnroDjQRR9DTjRTqpFTT9lm8t` &&
                          `ONCdeG67NlRmsDMeB539XC6WINEmTOUeQm8vGiKRuGWZd+uNzNnuBCrmEpL7hbFZBeqa3RNgSRLq034stuTPCOBSgzCf3Ggu8dsWNrLv7VTN1/ofAD8A89fyZNDp99AAAAAASUVORK5CYII=` )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_values = VALUE #(
                ( name = zif_sat_c_object_search=>c_visibility-public    description = 'Public' )
                ( name = zif_sat_c_object_search=>c_visibility-protected description = 'Protected' )
                ( name = zif_sat_c_object_search=>c_visibility-private   description = 'Private' ) ) ) ).
  ENDMETHOD.

  METHOD get_level_filter.
    result = VALUE #(
        name             = c_method_options-level
        description      = 'Declaration Level'
        long_description = |Use '{ c_method_options-level }' to limit result to methods defined at a certain level.\n\n| &&
                           |Example:\n   { c_method_options-level } : instance|
        img_info         = VALUE #(
            img_key     = c_image_keys-level
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABFklEQVR4nL2Qz07CQBDGK159E+2b9OibIIdSEho0teIfmgrssksNV6CtTdRSuHColPgAPXngKQiC2YwLIcaDh21NnGQyc5jvN/ONJP1X` &&
                          `zA/k0bwgwz4r2QFcOJWOj7bJ+8/cgMzCb8BPC4cncW7Q3gJkv6Agr/4KON9Cdha4nV+HCCEK6T6khDofvL5TSk+FNyBElDairBeMmT99g+FzBKhD1y2MxSB2E6dB8MSSJIHX2QziOAbXewTLRgshgHl7` &&
                          `v3wJQxhPJjCKIgh57/sBmDf2RghwYVopdXrM9TwYDF3o9weAOw7UjIbYBRW9rmh6nVl2m2HShTurBZp+vVarV+KPLKmGUtIu07OyuSmWjUVRNcTFeeML2CKeQ/aP3GQAAAAASUVORK5CYII=` )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_values = VALUE #(
                ( name = zif_sat_c_object_search=>c_method_level-instance description = 'Instance Method' )
                ( name = zif_sat_c_object_search=>c_method_level-static   description = 'Class Method' ) ) ) ).
  ENDMETHOD.

  METHOD get_status_filter.
    result = VALUE #(
        name             = c_method_options-status
        description      = 'Method Status'
        long_description = |Use '{ c_method_options-status }' to limit results to methods by status.\n\n| &&
                           |Example:\n   { c_method_options-status } : implemented|
        img_info         = VALUE #(
            img_key     = c_image_keys-status
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAOwAAADsAEnxA+tAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAK1JREFUOI3V0T1qgkEQh/Ff/ABb` &&
                          `A5Ib2KqX8C4B09pZWAZs7CwFS/EGnkFvIESw8AYiwlpki2V5lQUrBwb+zMczs7O8vX0k+hv9wr49Fjnghnoh4IYm1JJg2nzCJuqAawZoVFFD4mN8xUkzDLN8qJp6wTb6Ei30MMEfjujiM9ZP81Xaif7F` &&
                          `GT/oYB3BT2+Ur3jwf+RVRS6UAALm8WkPAS9/Y3qDEQaFgF1h3TvYHfH+NAnKJEY1AAAAAElFTkSuQmCC` )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_values = VALUE #(
                ( name = zif_sat_c_object_search=>c_method_status-standard    description = 'Defined in Class/Interface' )
                ( name = zif_sat_c_object_search=>c_method_status-implemented description = 'Implemented from Interface' )
                ( name = zif_sat_c_object_search=>c_method_status-redefined   description = 'Redefined from Super Class' ) ) ) ).
  ENDMETHOD.

  METHOD get_type_filter.
    result = VALUE #(
        name             = c_general_options-type
        description      = 'Method Type'
        long_description = |Use '{ c_general_options-type }' to find methods of a concrete type.\n\n| &&
                           |Example:\n   { c_general_options-type } : event_handler|
        img_info         = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_values = VALUE #(
                ( name = zif_sat_c_object_search=>c_method_types-general            description = 'General Method' )
                ( name = zif_sat_c_object_search=>c_method_types-constructor        description = 'Constructor' )
                ( name = zif_sat_c_object_search=>c_method_types-event_handler      description = 'Event Handler Method' )
                ( name = zif_sat_c_object_search=>c_method_types-virtual_getter     description = 'Get Method of a virtual attribute' )
                ( name = zif_sat_c_object_search=>c_method_types-virtual_setter     description = 'Set Method of a virtual attribute' )
                ( name = zif_sat_c_object_search=>c_method_types-test               description = 'Test method for ABAP Unit' ) )
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) ).
  ENDMETHOD.
ENDCLASS.
