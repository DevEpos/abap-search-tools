"! <p class="shorttext synchronized" lang="en">Query Config for Method search</p>
CLASS zcl_sat_clif_meth_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_clsintf_query_config
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      zif_sat_object_search_config~get_type
        REDEFINITION,
      zif_sat_object_search_config~get_option_config
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_option_target_method TYPE string VALUE 'meth'.
    DATA:
      mt_method_options        TYPE zif_sat_object_search_config=>ty_t_option_setting,
      mv_option_prefix_pattern TYPE string.
ENDCLASS.



CLASS zcl_sat_clif_meth_query_config IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    DELETE mt_options WHERE option = c_class_intf_search_option-attribute
                         OR option = c_class_intf_search_option-method.

    " TODO: add filters for method:
    " - param (any)
    " - type (constructor,handler)
    " - flag (optional,abstract,final,class_exceptions)
    " - exposure (private,protected,public)
    " - level (static, instance)
    " - desc (description of method)
  ENDMETHOD.


  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-method.
  ENDMETHOD.


  METHOD zif_sat_object_search_config~get_option_config.
    IF iv_target = c_option_target_method.
      rs_option = mt_method_options[ option = iv_option ].
    ELSE.
      rs_option = super->zif_sat_object_search_config~get_option_config( iv_option ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
