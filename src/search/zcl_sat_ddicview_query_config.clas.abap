"! <p class="shorttext synchronized">Configuration for Database Table/View Query</p>
CLASS zcl_sat_ddicview_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_os_view_options.

    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    ALIASES c_view_search_options FOR zif_sat_c_os_view_options~c_filter_key.

    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        transport      TYPE string VALUE 'ABAP:IMG_TRANSPORT',
        buffering      TYPE string VALUE 'ABAP:IMG_BUFFERING',
        buffering_type TYPE string VALUE 'ABAP:IMG_BUFFERING_TYPE',
        data_class     TYPE string VALUE 'ABAP:IMG_DATA_CLASS',
        size_category  TYPE string VALUE 'ABAP:IMG_SIZE_CATEGORY',
      END OF c_image_keys.

    METHODS get_field_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_deliv_class_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_root_tab_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_base_tab_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_maintenance_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_ddicview_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-ddic_view.
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
        ( get_rel_state_filt_conf( iv_object_filter = zif_sat_c_object_types=>view_definition ) )
        ( get_root_tab_filt_conf( ) )
        ( get_base_tab_filt_conf( ) )
        ( get_type_filter( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( get_field_filter( ) )
        ( get_deliv_class_filter( ) )
        ( get_maintenance_filter( ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_search_type_config(
        label    = 'View'
        name     = zif_sat_c_object_search=>c_search_type-ddic_view
        img_info = VALUE #( img_key      = zif_sat_c_object_types=>view_definition
                            img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD get_type_filter.
    result = VALUE #(
        name             = c_general_options-type
        description      = 'View Class'
        long_description = |Use '{ c_general_options-type }' to restrict the search query to views of a certain type.\n\n| &&
                           |Example:\n   { c_general_options-type } : help|
        img_info         = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_values = VALUE #(
                ( name = zif_sat_c_os_view_options=>c_view_class-ext-database    description = 'Database View' )
                ( name = zif_sat_c_os_view_options=>c_view_class-ext-help        description = 'Help View' )
                ( name = zif_sat_c_os_view_options=>c_view_class-ext-projection  description = 'Projection View' )
                ( name = zif_sat_c_os_view_options=>c_view_class-ext-maintenance description = 'Maintenance View' )
                ( name = zif_sat_c_os_view_options=>c_view_class-ext-external    description = 'External View (HANA)' ) )
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) )  ) ).
  ENDMETHOD.

  METHOD get_deliv_class_filter.
    result = VALUE #(
        name             = c_view_search_options-delivery_class
        description      = 'Delivery Class'
        long_description = |Use '{ c_view_search_options-delivery_class }' to search for views with specific delivery classes.\n\n| &&
                           |Example:\n   { c_view_search_options-delivery_class } : A|
        img_info         = VALUE #(
            img_key     = c_image_keys-transport
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABXElEQVR4nKWRXUvCcBTG/WBdlSBEEQRCJERvBN3UTYRERGWElJpRWpRSggZtZS+uV4Vd1G2Us75Ca5pGb2tq9bT9w6lbLcIDP3ZxHn47| &&
                          |5/xNplqLPXTiLwwFpzEXLrLAuQCc/UCcmTUWxKLzJJiRgGxBD7PjNRbQlJ8IcnL4sahni/YZC8KRVSJ4kMPP73oiGytE0DVzjF7XCfo8cfR7ExhcZJuIIBBaJ4InWSB+6AmG1lTBZbZIYK7TiuB7Ml8g| &&
                          |QgQv8t/yn3r8wXBZcF9QUQXuJQruZQqeKjar0K0wl8DAAmt8G21VCpRSBeZhChY7jcaRbTSPRtEytovW8T1YJ/fRNsXANn2ADucRfp1AESg7XZXI5JFMS0gKEjjhDdydCI4XkeJfkboto96gNIG5cwJ1| &&
                          |1iHy1U5g6XGQnqXbUfmM1TdQAnw6R4La/Y16ajXY7JwSqG+33/ynV3N9AVC53MX4NLXNAAAAAElFTkSuQmCC| )
        allowed_length   = 1
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            caching               = abap_true
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-table_deliv_class
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_root_tab_filt_conf.
    result = VALUE #(
        name             = c_view_search_options-primary_table
        description      = 'Primary Table'
        long_description = |Use '{ c_view_search_options-primary_table }' to restrict the search query to Views which have | &&
                           |a certain primary Table/View .\n\nExample:\n   { c_view_search_options-primary_table } : mara|
        img_info         = VALUE #( img_key     = c_general_image_keys-table_source
                                    img_encoded = get_general_image( c_general_image_keys-table_source ) )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term   = zif_sat_c_object_search=>c_content_assist-terms-view_root_tab
            proposal_images = VALUE #( ( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                         img_key      = zif_sat_c_object_types=>table_definition  ) ) ) ).
  ENDMETHOD.

  METHOD get_base_tab_filt_conf.
    result = VALUE #(
        name             = c_view_search_options-base_table
        description      = 'Base Table'
        long_description = |Use '{ c_view_search_options-base_table }' to restrict the search query to Views which have | &&
                           |a certain base Table/View .\n\nExample:\n   { c_view_search_options-base_table } : mara|
        img_info         = VALUE #(
            img_key     = 'ABAP:IMG_VIEW_BASE_TAB'
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAE9SURBVDhPtZO9SsRAFIXP/GZDDKSysIi9WCho| &&
                          |o5Y2Fm5loZUPYGMh1uITWFlbWNhqYyNY+ATCdoqixeJPhAXFDZtkx0s2LG4SEmHxm2KGe+49M/eGYFxYtg85utycZqp3SsdFwFiDFEMLEQO7NZHe2F07e0qTCZ7tQ6j4njOxYinHmvJmMec34TYmYStX| &&
                          |CS4XSH/IUlMKBgZ9KbgCZwqO9uh+Dlu7YEymMdJHaooGxqDz/YYw+kSrfYWbuxM8f7TwFQbodF9S/TflBt13aDmB1ZkdbC8dY95fhxQWGQf1Bn1aVeT1khmM3pAnr5e2UMWfZlBFvUHNDPL6f7RQ84Kc| &&
                          |XtbCo62dJEpCxElvEDMJEhOjoeyY9Nc0mFH4mQ4ulj0h9Z4wYose69OXlzAsZhxtKj5XMQ73m9dBlj4uwA+K6oS4j6umkwAAAABJRU5ErkJggg==| )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term   = zif_sat_c_object_search=>c_content_assist-terms-view_base_tab
            proposal_images = VALUE #( ( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                         img_key      = zif_sat_c_object_types=>table_definition  ) ) ) ).
  ENDMETHOD.

  METHOD get_field_filter.
    result = VALUE #(
        name             = c_view_search_options-field
        description      = 'View Field'
        long_description = |Use '{ c_view_search_options-field }' to restrict the search query by certain Fields.\n\n| &&
             |Example:\n   { c_view_search_options-field } : devclass|
        img_info         = VALUE #( img_key      = zif_sat_c_object_types=>table_field
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        patterns         = abap_true
        allowed_length   = 30
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-view_field
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_maintenance_filter.
    result = VALUE #(
        name             = c_general_options-maintenance
        description      = 'Maintenance with Standard Tools'
        long_description = |Use '{ c_general_options-maintenance }' to restrict the query by the view maintenance setting.\n\n| &&
                           |Example:\n   { c_general_options-maintenance } : not_allowed|
        img_info         = VALUE #( img_key     = c_general_image_keys-maintenance
                                    img_encoded = get_general_image( c_general_image_keys-maintenance ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = zcl_sat_table_filter_values=>get_maintenance_filt_values( ) ) ).
  ENDMETHOD.
ENDCLASS.
