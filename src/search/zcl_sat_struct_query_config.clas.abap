"! <p class="shorttext synchronized">Configuration for Database Table/View Query</p>
CLASS zcl_sat_struct_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_os_dtab_options.

    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    ALIASES c_dbtab_options FOR zif_sat_c_os_dtab_options~c_filter_key.

    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    METHODS get_field_filter
      RETURNING VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_enhancement_cat_filter
      RETURNING VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_include_filter
      RETURNING VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_struct_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-structure.
  ENDMETHOD.

  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_query_filters(
        ( get_user_filt_conf( ) )
        ( get_created_on_filt_conf( ) )
        ( get_changed_by_filt_conf( ) )
        ( get_changed_on_filt_conf( ) )
        ( get_package_filt_conf( ) )
        ( get_softw_comp_filt_conf( ) )
        ( get_appl_comp_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( get_field_filter( ) )
        ( get_include_filter( ) )
        ( get_enhancement_cat_filter( ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_search_type_config(
        label    = 'Structure'
        name     = zif_sat_c_object_search=>c_search_type-structure
        img_info = VALUE #( img_key      = zif_sat_c_object_types=>structure
                            img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD get_field_filter.
    result = VALUE #(
        name             = c_dbtab_options-field
        description      = 'Structure Field'
        long_description = |Use '{ c_dbtab_options-field }' to restrict the search query by certain Fields.\n\n| &&
             |Example:\n   { c_dbtab_options-field } : devclass|
        img_info         = VALUE #( img_key      = zif_sat_c_object_types=>table_field
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        patterns         = abap_true
        allowed_length   = 30
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-structure_field
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_enhancement_cat_filter.
    result = VALUE #(
        name             = c_dbtab_options-enhancement_category
        description      = 'Enhancement Category'
        long_description = |Use '{ c_dbtab_options-enhancement_category }' to find structures by their enhancement categories.\n\n| &&
                           |Example:\n   { c_dbtab_options-enhancement_category } : not_enhanced|
        img_info         = VALUE #( img_key     = c_general_image_keys-extension
                                    img_encoded = get_general_image( c_general_image_keys-extension ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = zcl_sat_table_filter_values=>get_enhancecat_filt_values( ) ) ).
  ENDMETHOD.

  METHOD get_include_filter.
    result = VALUE #(
        name             = c_dbtab_options-include_usage
        description      = 'Usages of include structures'
        long_description = |Use '{ c_dbtab_options-include_usage }' to restrict the query to structures that have specific includes.\n\n| &&
                           |Example:\n   { c_dbtab_options-include_usage } : seochange|
        patterns         = abap_true
        img_info         = VALUE #( img_key      = zif_sat_c_object_types=>structure
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-structure_include
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.
ENDCLASS.
