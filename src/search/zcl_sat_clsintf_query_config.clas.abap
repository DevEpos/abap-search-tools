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
      ( option = c_search_option-by_owner allowed_length = 12 )
      ( option = c_search_option-by_package allowed_length = 30 )
      ( option = c_search_option-by_type )
      ( option = c_search_option-by_api )
      ( option = c_search_option-by_description allowed_length = 40 )
      ( option = c_search_option-max_rows single = abap_true no_negation = abap_true )
      ( option = c_class_intf_search_option-by_method allowed_length = 30 )
      ( option = c_class_intf_search_option-by_attribute allowed_length = 30 )
      ( option = c_class_intf_search_option-by_friend allowed_length = 30 )
      ( option = c_class_intf_search_option-by_sub_type allowed_length = 30 )
      ( option = c_class_intf_search_option-by_super_type allowed_length = 30 )
    ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-class_interface.
  ENDMETHOD.

ENDCLASS.
