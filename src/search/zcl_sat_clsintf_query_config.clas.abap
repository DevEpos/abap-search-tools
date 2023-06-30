"! <p class="shorttext synchronized">Configuration for Class/Interface Query</p>
CLASS zcl_sat_clsintf_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.

    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type          REDEFINITION.
    METHODS zif_sat_object_search_config~get_search_config REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_clsintf_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter(
        ( get_user_filt_conf( ) )
        ( get_package_filt_conf( ) )
        ( get_rel_state_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( name           = c_general_options-type
          caching        = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_type ) )
        ( name           = c_class_intf_search_option-flag
          caching        = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_flag ) )
        ( name           = c_class_intf_search_option-category
          caching        = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_category ) )
        ( name           = c_class_intf_search_option-abap_lang
          caching        = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = c_vh_category_scheme
                                    category_term   = 'classcategory' ) )
        ( option = c_class_intf_search_option-method allowed_length = 61 )
        ( option = c_class_intf_search_option-interface allowed_length = 30 )
        ( option = c_class_intf_search_option-attribute allowed_length = 30 key_value = abap_true )
        ( option = c_class_intf_search_option-friend allowed_length = 30 )
        ( option = c_class_intf_search_option-super_type allowed_length = 30 ) ).

    ms_search_type = VALUE #(
        label  = 'Class/Interface'
        name   = zif_sat_c_object_search=>c_search_type-class_interface
        inputs = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                            label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                          ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                            label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                            filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-class_interface.
  ENDMETHOD.
ENDCLASS.
