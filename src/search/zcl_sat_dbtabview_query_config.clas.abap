"! <p class="shorttext synchronized">Configuration for Database Table/View Query</p>
CLASS zcl_sat_dbtabview_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    ALIASES c_dbtab_options FOR zif_sat_c_object_search~c_dbtab_search_params.

    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        transport TYPE string VALUE 'ABAP:IMG_TRANSPORT',
      END OF c_image_keys.

    METHODS get_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sat_dbtabview_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter(
        ( get_package_filt_conf( ) )
        ( get_user_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( name           = c_dbtab_options-field
          img_info       = VALUE #( img_key      = zif_sat_c_object_types=>table_field
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
          allowed_length = 30
          content_assist = VALUE #(
              assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term         = zif_sat_c_object_search=>c_content_assist-terms-table_field
              proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) )
        ( name           = c_general_options-type
          img_info       = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
          content_assist = VALUE #(
              assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              caching         = abap_true
              category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_type
              proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                           img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) )
        ( name           = c_dbtab_options-delivery_class
          img_info       = VALUE #( img_key     = c_image_keys-transport
                                    img_encoded = get_image( c_image_keys-transport ) )
          allowed_length = 1
          content_assist = VALUE #(
              assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              caching               = abap_true
              category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term         = zif_sat_c_object_search=>c_content_assist-terms-table_deliv_class
              proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_s_search_type(
        label    = 'Database Table/View'
        name     = zif_sat_c_object_search=>c_search_type-db_tab_view
        img_info = VALUE #( img_key      = zif_sat_c_object_types=>table_definition
                            img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-db_tab_view.
  ENDMETHOD.

  METHOD get_image.
    CASE iv_image_key.

      WHEN c_image_keys-transport.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABXElEQVR4nKWRXUvCcBTG/WBdlSBEEQRCJERvBN3UTYRERGWElJpRWpRSggZtZS+uV4Vd1G2Us75Ca5pGb2tq9bT9w6lbLcIDP3ZxHn475/xNplqLPXTiLwwFpzEXLrLAuQCc/UCcmTUWxKLzJJiRg` &&
                 `GxBD7PjNRbQlJ8IcnL4sahni/YZC8KRVSJ4kMPP73oiGytE0DVzjF7XCfo8cfR7ExhcZJuIIBBaJ4InWSB+6AmG1lTBZbZIYK7TiuB7Ml8gQgQv8t/yn3r8wXBZcF9QUQXuJQruZQqeKjar0K0wl8DAAmt8G21VCpRSBeZhChY7jcaRbTSPRtEytovW8T1YJ/fRNs` &&
                 `XANn2ADucRfp1AESg7XZXI5JFMS0gKEjjhDdydCI4XkeJfkboto96gNIG5cwJ11iHy1U5g6XGQnqXbUfmM1TdQAnw6R4La/Y16ajXY7JwSqG+33/ynV3N9AVC53MX4NLXNAAAAAElFTkSuQmCC`.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
