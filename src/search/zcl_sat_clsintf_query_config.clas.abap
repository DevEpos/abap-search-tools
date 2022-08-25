"! <p class="shorttext synchronized" lang="en">Configuration for Class/Interface Query</p>
CLASS zcl_sat_clsintf_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC .

  PUBLIC SECTION.
    ALIASES:
      c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_clsintf_query_config IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mt_options = VALUE #(
      ( option = c_general_options-user allowed_length = 12 )
      ( option = c_general_options-package allowed_length = 30 )
      ( option = c_general_options-type )
      ( option = c_general_options-release_state )
      ( option = c_general_options-description allowed_length = 40 )
      ( option = c_general_options-max_rows single = abap_true no_negation = abap_true )
      ( option = c_class_intf_search_option-flag )
      ( option = c_class_intf_search_option-category )
      ( option = c_class_intf_search_option-method allowed_length = 61 )
      ( option = c_class_intf_search_option-interface allowed_length = 30 )
      ( option = c_class_intf_search_option-attribute allowed_length = 30 key_value = abap_true )
      ( option = c_class_intf_search_option-friend allowed_length = 30 )
      ( option = c_class_intf_search_option-super_type allowed_length = 30 )
      ( option = c_class_intf_search_option-abap_lang ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-class_interface.
  ENDMETHOD.

ENDCLASS.
