"! <p class="shorttext synchronized">Configuration for CDS View Query</p>
CLASS zcl_sat_cds_view_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_os_cds_options.

    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_cds_options FOR zif_sat_c_os_cds_options~c_filter_key.

    CONSTANTS:
      BEGIN OF c_image_keys,
        association TYPE string VALUE 'ABAP:IMG_ASSOC',
        base_field  TYPE string VALUE 'ABAP:IMG_BASE_FIELD',
        anno        TYPE string VALUE 'ABAP:IMG_ANNO',
        db_entity   TYPE string VALUE 'ABAP:IMG_DB_ENTITY',
      END OF c_image_keys.

    METHODS get_param_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_params_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_from_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_association_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_annotation_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_field_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_cds_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_ext_by_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_base_field_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_cds_view_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_query_filters( ( get_user_filt_conf( ) )
                                                                                ( get_created_on_filt_conf( ) )
                                                                                ( get_changed_by_filt_conf( ) )
                                                                                ( get_changed_on_filt_conf( ) )
                                                                                ( get_package_filt_conf( ) )
                                                                                ( get_softw_comp_filt_conf( ) )
                                                                                ( get_appl_comp_filt_conf( ) )
                                                                                ( get_cds_type_filter( ) )
                                                                                ( get_rel_state_filt_conf( ) )
                                                                                ( get_description_filt_conf( ) )
                                                                                ( get_max_rows_filt_conf( ) )
                                                                                ( get_params_filter( ) )
                                                                                ( get_param_filter( ) )
                                                                                ( get_from_filter( ) )
                                                                                ( get_association_filter( ) )
                                                                                ( get_annotation_filter( ) )
                                                                                ( get_field_filter( ) )
                                                                                ( get_base_field_filter( ) )
                                                                                ( get_ext_by_filter( ) ) ).

    mt_options = lt_object_filters.

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_search_type_config(
        label          = 'CDS View'
        name           = zif_sat_c_object_search=>c_search_type-cds_view
        custom_options = VALUE #(
            ( key         = zif_sat_c_os_cds_options=>c_custom_options-resolve_from_hierarchy
              type        = zif_sat_c_object_search=>c_custom_option_data_type-boolean
              label       = 'Consider Indirect Select Sources of "from" Filter'
              description = |Loads Where-Used Tree of single "from" filter to restrict search scope to that hierarchy.\n\n| &&
                            |Note: The option will only be used if a single value for "from" was provided (Wildcards/Exclusion not allowed)| ) )
        img_info       = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                  img_key      = zif_sat_c_object_types=>structured_object )
        inputs         = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                    label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                                  ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                    label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                                    filters = lt_object_filters ) ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-cds_view.
  ENDMETHOD.

  METHOD get_annotation_filter.
    result = VALUE #(
        name             = c_cds_options-annotation
        description      = 'Annotation'
        long_description = |Use '{ c_cds_options-annotation }' to restrict the search query by certain Annotations.\n| &&
                           |This parameter also allows the input with Key/Value pattern.\n\nExample:\n   { c_cds_options-annotation } : analytics.query\n\n| &&
                           |Example with Key/Value:\n   { c_cds_options-annotation } : searchable=true|
        img_info         = VALUE #(
            img_key     = c_image_keys-anno
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACXUlEQVR4nGNgwAJcWjbbuLVt6Xdt3XIZiO8D8XWX1q3rgXSMQ8N+Dmx6wMCtdYsOUNF5IH4MMWBrgkvzFhe3tq0+QEObQeIgA11atllg` &&
                          `aPbu3qnj1bn9tVfX9nagLTzYLADZ7tOxvdm7c/t3n/btDiiSAd07Twf07uyH8/t3GQT27cgI6t3ZENSzKyWoc5sMTC6oZ3dzYO/O5wH9+wXAAuH9e1PCJuz5HtmzXwSmKGzinudhk/asD5+wZ37YxL2X` &&
                          `wyfueR8+abcF1CUsQPnHERP3loAVB/ft2h/cv2s+rrABaQjq3XUZiOFqAnt3LQbi42COb9f2575d20rQNXl373AByiWA/OvTuX25T9f2/TB5IL8BGBb3IaHftvU/KMRhku7tO1TcWrdeB+L7rm1b9gNj` &&
                          `5zYQ/wexYWrcWjc3gGIEYlvjps8OTRtzELZv6geK3XZo2AwPE/uGTfPtmzbtR/A3LwfyN4M5VrUbrlvWbpgOk7SqXTfbqm79aYSB8zmA/MOWtevBBljWb1SxrFn/2bJmnQ9YgUnVmunGlWvvg/wN4htX` &&
                          `rgkwqVz736Ry9Wogu9u4Ys11MC5f896kYnUDkH3fpHw1PMoZdIvXaOiVrvquV7oSHpA6JasddEtWTQZhEFs7axWPbvHqGt3iVfP1SlZHwCyDA/XC5RkaBcv/qxcsK8CQJBYo5i4JUcxZ+l4xZ8luxdzF` &&
                          `OUpZy2wUCpYrgLBy3iIdheylEUD5yQq5SxNwGiKVtkxEOnNRjVTmou3S6YveS2cs+g/E34H4MZC/XypjUbt06nwDslyIDQAAQgkg3iT77dwAAAAASUVORK5CYII=` )
        key_value        = abap_true
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type             = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme         = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term           = zif_sat_c_object_search=>c_content_assist-terms-annotation
            secondary_category_term = COND #( WHEN sy-saprl > 751
                                              THEN zif_sat_c_object_search=>c_content_assist-terms-annotatio_value )
            proposal_image_source   = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter  ) ).
  ENDMETHOD.

  METHOD get_association_filter.
    result = VALUE #(
        name             = c_cds_options-association
        description      = 'Association'
        long_description = |Use '{ c_cds_options-association }' to restrict the search query to CDS views which use | &&
                           |a certain Table/View/CDS view as an Association.\n\nExample:\n   { c_cds_options-association } : i_product|
        img_info         = VALUE #(
            img_key     = c_image_keys-association
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABBElEQVR4nGNgoAYQaD+vINJ69qBI65l3oq2nj4L4JBkg1n72RPbWh397jz3/n7D+3l+x1tPHSDOg7cxH+wXX//suv/Xfbv71/6Itp74z` &&
                          `hM78jxOjA/GOs6cNZl75bzr76n8QLdZ+5jhO27AZAPKzaNvZ06JtZz6Jt509gzcMsBlAEiDbgNCZk1HCIWTWdNIMcGhgAWrcDjFg1n6GhPkcZLhiKg9D6IzNDJEzRUjWCwpYyc5zV5R6z/+Q7Dhzg5zE` &&
                          `djlry/3/fcdBie3uf9H2sxdIMgAYzV8s513777To+n8QDeR/INUFJwxmXv1jAk5sV/+ItZGY3OEZru3MO1BeITkMcAEAZhGbMbBbTB8AAAAASUVORK5CYII=` )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type             = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme         = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term           = zif_sat_c_object_search=>c_content_assist-terms-db_entity
            proposal_image_source   = zif_sat_c_object_search=>c_proposal_image_source-proposal
            proposal_image_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type ) ).
  ENDMETHOD.

  METHOD get_cds_type_filter.
    result = VALUE #(
        name             = c_general_options-type
        description      = 'Source Type'
        long_description = |Use '{ c_general_options-type }' to restrict the search query to CDS views which are of a certain type.\n\n| &&
                           |Example:\n   { c_general_options-type } : function|
        img_info         = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            caching         = abap_true
            category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_type
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) ).
  ENDMETHOD.

  METHOD get_ext_by_filter.
    result = VALUE #(
        name             = c_cds_options-extended_by
        description      = 'Extended By'
        long_description = |Use '{ c_cds_options-extended_by }' to restrict the search query to CDS views which are | &&
                           |extended by certain Extension views.\n\nExample:\n   { c_cds_options-extended_by } : i_material|
        img_info         = VALUE #( img_key     = c_general_image_keys-extension
                                    img_encoded = get_general_image( c_general_image_keys-extension ) )
        allowed_length   = 30
        patterns         = abap_true
        no_negation      = abap_true
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_extension
            proposal_images = VALUE #( ( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                         img_key      = zif_sat_c_object_types=>structured_object  ) ) ) ).
  ENDMETHOD.

  METHOD get_field_filter.
    result = VALUE #(
        name             = c_cds_options-field
        description      = 'CDS Field'
        long_description = |Use '{ c_cds_options-field }' to restrict the search query by certain Fields.\n\n| &&
                           |Example:\n   { c_cds_options-field } : material|
        img_info         = VALUE #( img_key      = zif_sat_c_object_types=>structured_object_field
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-cds_field
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_base_field_filter.
    result = VALUE #(
        name             = c_cds_options-base_field
        description      = 'CDS Base Field'
        long_description = |Use '{ c_cds_options-base_field }' to restrict the search query by certain base fields.\n\n| &&
                           |Example:\n   { c_cds_options-base_field } : matnr|
        img_info         = VALUE #(
            img_key     = c_image_keys-base_field
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAOwQAADsEBuJFr7QAAABh0RVh0U29mdHdhcmUAcGFpbnQubmV0IDQuMS42/U4J6AAAAJRJREF` &&
                          `UOE9joCqYveXOf3QMlYIDE/8qDDE4AGk49+gvHKMbANLslb/2P05DYAbceQXByAbANMMwVkNgBoBoGIZKwQFIM5SJCWAGgGx//xXTCyBAsQGhDccIGwCiYRgqBQdEGUCxC/AZkL3oJ24DkKMJhqFScE` &&
                          `DQgN03/oLxw7d/yTMApBGGyTIAHUOl4ACvAQMAGBgAyskA69j6uwoAAAAASUVORK5CYII=`  )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-cds_base_field
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_from_filter.
    result = VALUE #(
        name             = c_cds_options-select_from
        description      = 'Select From'
        long_description = |Use '{ c_cds_options-select_from }' to restrict the search query to CDS views which use | &&
                           |a certain Table/View/CDS View in their SELECT clause.\n\nExample:\n   { c_cds_options-select_from } : mara|
        img_info         = VALUE #( img_key     = c_general_image_keys-table_source
                                    img_encoded = get_general_image( c_general_image_keys-table_source ) )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type             = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme         = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term           = zif_sat_c_object_search=>c_content_assist-terms-db_entity
            proposal_image_source   = zif_sat_c_object_search=>c_proposal_image_source-proposal
            proposal_image_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type ) ).
  ENDMETHOD.

  METHOD get_params_filter.
    result = VALUE #(
        name             = c_cds_options-params
        description      = 'Parameters Check'
        long_description = |Use '{ c_cds_options-params }' to restrict the search query to CDS views which have (not) parameters.\n\n| &&
                           |Example:\n   { c_cds_options-params } : true|
        img_info         = VALUE #( img_key     = c_general_image_keys-param
                                    img_encoded = get_general_image( c_general_image_keys-param ) )
        single           = abap_true
        no_negation      = abap_true
        data_type        = zif_sat_c_object_search=>c_filter_data_type-boolean ).
  ENDMETHOD.

  METHOD get_param_filter.
    result = VALUE #(
        name             = c_cds_options-param
        description      = 'CDS Parameter'
        long_description = |Use '{ c_cds_options-param }' to restrict the search query to CDS views which have a certain parameter.\n\n| &&
                           |Example:\n   { c_cds_options-param } : p_test|
        img_info         = VALUE #( img_key     = c_general_image_keys-param
                                    img_encoded = get_general_image( c_general_image_keys-param ) )
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-cds_param
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.
ENDCLASS.
