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
    DATA ms_search_type TYPE zif_sat_ty_object_search=>ty_s_search_type.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_clsintf_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_option_setting(
        ( option         = c_general_options-user
          allowed_length = 12
          content_assist = VALUE #( assist_type = zif_sat_c_object_search=>c_filter_content_assist_type-user ) )
        ( option         = c_general_options-package
          allowed_length = 30
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-ris
                                    adt_object_type = 'DEVC/K' ) )
        ( option         = c_general_options-type
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = c_vh_category_scheme
                                    category_term   = 'classtype' ) )
        ( option         = c_general_options-release_state
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = c_vh_category_scheme
                                    category_term   = 'releasestate' ) )
        ( option = c_general_options-description allowed_length = 40 )
        ( option = c_general_options-max_rows single = abap_true no_negation = abap_true )
        ( option         = c_class_intf_search_option-flag
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = c_vh_category_scheme
                                    category_term   = 'classflag' ) )
        ( option         = c_class_intf_search_option-category
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = c_vh_category_scheme
                                    category_term   = 'classcategory' ) )
        ( option = c_class_intf_search_option-method allowed_length = 61 )
        ( option = c_class_intf_search_option-interface allowed_length = 30 )
        ( option = c_class_intf_search_option-attribute allowed_length = 30 key_value = abap_true )
        ( option = c_class_intf_search_option-friend allowed_length = 30 )
        ( option = c_class_intf_search_option-super_type allowed_length = 30 )
        ( option         = c_class_intf_search_option-abap_lang
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = c_vh_category_scheme
                                    category_term   = 'abaplanguage' ) ) ).

    ms_search_type = VALUE #( label  = 'Class/Interface'
                              name   = 'clif'
                              inputs = VALUE #( ( name    = c_object_name_input_key
                                                  label   = c_object_name_input_label )
                                                ( name    = c_object_filter_input_key
                                                  label   = c_object_filter_input_label
                                                  filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-class_interface.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_search_config.
    result = ms_search_type.
  ENDMETHOD.
ENDCLASS.
