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
    METHODS zif_sat_object_search_config~has_option        REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        param TYPE string VALUE 'ABAP:IMG_',
      END OF c_image_keys.

    DATA mt_method_options        TYPE zif_sat_ty_object_search=>ty_t_query_filter.
    DATA mv_option_prefix_pattern TYPE string.

    METHODS get_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS delete_invalid_obj_filters.
ENDCLASS.


CLASS zcl_sat_clif_meth_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD build_config.
    super->build_config( ).

    delete_invalid_obj_filters( ).

    " TODO: add filters for method:
    " - param (any)
    " - type (constructor,handler,...)
    " - flag (optional,abstract,final,class_exceptions)
    " - exposure (private,protected,public)
    " - level (static, instance)
    " - desc (description of method)

    DATA(lt_method_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter( ( get_description_filt_conf( ) ) ).

    ms_search_type-label    = 'Method'.
    ms_search_type-img_info = VALUE #( img_key     = c_clif_image_keys-method
                                       img_encoded = get_clif_image( c_clif_image_keys-method ) ).
    ms_search_type-name     = zif_sat_c_object_search=>c_search_type-method.
    ms_search_type-inputs   = VALUE #( BASE ms_search_type-inputs
                                       ( name    = zif_sat_c_object_search=>c_search_fields-method_name_input_key
                                         label   = zif_sat_c_object_search=>c_search_fields-method_name_input_label )
                                       ( name    = zif_sat_c_object_search=>c_search_fields-method_filter_input_key
                                         label   = zif_sat_c_object_search=>c_search_fields-method_filter_input_label
                                         filters = lt_method_filters ) ).
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

  METHOD zif_sat_object_search_config~has_option.
    IF iv_target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.
      rf_has_option = xsdbool( line_exists( mt_method_options[ name = iv_option ] ) ).
    ELSE.
      rf_has_option = super->zif_sat_object_search_config~has_option( iv_option = iv_option iv_target = iv_target ).
    ENDIF.
  ENDMETHOD.

  METHOD get_image.
    CASE iv_image_key.

      WHEN c_image_keys-param.
        result = ``.

    ENDCASE.
  ENDMETHOD.

  METHOD delete_invalid_obj_filters.
    DELETE mt_options WHERE    name = c_class_intf_search_option-attribute
                            OR name = c_class_intf_search_option-method.

    DATA(lr_object_filters) = REF #( ms_search_type-inputs[
                                         name = zif_sat_c_object_search=>c_search_fields-object_filter_input_key ]-filters ).

    DELETE lr_object_filters->* WHERE    name = c_class_intf_search_option-attribute
                                      OR name = c_class_intf_search_option-method.
  ENDMETHOD.
ENDCLASS.
