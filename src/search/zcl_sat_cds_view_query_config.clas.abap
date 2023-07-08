"! <p class="shorttext synchronized">Configuration for CDS View Query</p>
CLASS zcl_sat_cds_view_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_cds_options FOR zif_sat_c_object_search~c_cds_search_params.

    CONSTANTS:
      BEGIN OF c_image_keys,
        from        TYPE string VALUE 'ABAP:IMG_FROM_PART',
        association TYPE string VALUE 'ABAP:IMG_ASSOC',
        anno        TYPE string VALUE 'ABAP:IMG_ANNO',
        extended_by TYPE string VALUE 'ABAP:IMG_EXT_BY',
        db_entity   TYPE string VALUE 'ABAP:IMG_DB_ENTITY',
      END OF c_image_keys.

    METHODS get_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zcl_sat_cds_view_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter(
        ( get_package_filt_conf( ) )
        ( get_user_filt_conf( ) )
        ( get_rel_state_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( name           = c_cds_options-params
          img_info       = VALUE #( img_key     = c_general_image_keys-param
                                    img_encoded = get_general_image( c_general_image_keys-param ) )
          single         = abap_true
          no_negation    = abap_true
          data_type      = zif_sat_c_object_search=>c_filter_data_type-boolean )
        ( name           = c_cds_options-param
          img_info       = VALUE #( img_key     = c_general_image_keys-param
                                    img_encoded = get_general_image( c_general_image_keys-param ) )
          patterns       = abap_true )
        ( name           = c_cds_options-select_from
          img_info       = VALUE #( img_key     = c_image_keys-from
                                    img_encoded = get_image( c_image_keys-from ) )
          allowed_length = 30
          patterns       = abap_true
          content_assist = VALUE #(
              assist_type             = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme         = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term           = zif_sat_c_object_search=>c_content_assist-terms-db_entity
              proposal_image_source   = zif_sat_c_object_search=>c_proposal_image_source-proposal
              proposal_image_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type ) )
        ( name           = c_cds_options-association
          img_info       = VALUE #( img_key     = c_image_keys-association
                                    img_encoded = get_image( c_image_keys-association ) )
          allowed_length = 30
          patterns       = abap_true
          content_assist = VALUE #(
              assist_type             = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme         = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term           = zif_sat_c_object_search=>c_content_assist-terms-db_entity
              proposal_image_source   = zif_sat_c_object_search=>c_proposal_image_source-proposal
              proposal_image_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type ) )
        ( name           = c_cds_options-annotation
          img_info       = VALUE #( img_key     = c_image_keys-anno
                                    img_encoded = get_image( c_image_keys-anno ) )
          key_value      = abap_true
          patterns       = abap_true )
        ( name           = c_cds_options-field
          img_info       = VALUE #( img_key      = zif_sat_c_object_types=>structured_object_field
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
          allowed_length = 30
          patterns       = abap_true
          content_assist = VALUE #(
              assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term         = zif_sat_c_object_search=>c_content_assist-terms-cds_field
              proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) )
        ( name           = c_general_options-type
          img_info       = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
          content_assist = VALUE #(
              assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              caching         = abap_true
              category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_type
              proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                           img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) )
        ( name           = c_cds_options-extended_by
          img_info       = VALUE #( img_key     = c_image_keys-extended_by
                                    img_encoded = get_image( c_image_keys-extended_by ) )
          allowed_length = 30
          patterns       = abap_true
          no_negation    = abap_true
          content_assist = VALUE #(
              assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_extension
              proposal_images = VALUE #( ( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                           img_key      = zif_sat_c_object_types=>structured_object ) ) ) ) ).

    mt_options = lt_object_filters.

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_s_search_type(
        label    = 'CDS View'
        name     = zif_sat_c_object_search=>c_search_type-cds_view
        img_info = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                            img_key      = zif_sat_c_object_types=>structured_object )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters ) ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-cds_view.
  ENDMETHOD.

  METHOD get_image.
    CASE iv_image_key.

      WHEN c_image_keys-anno.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACXUlEQVR4nGNgwAJcWjbbuLVt6Xdt3XIZiO8D8XWX1q3rgXSMQ8N+Dmx6wMCtdYsOUNF5IH4MMWBrgkvzFhe3tq0+QEObQeIgA11atllgaPbu3qnj1bn9tVfX9nagLTzYLADZ7tOxvdm7c/t3n/b` &&
                 `tDiiSAd07Twf07uyH8/t3GQT27cgI6t3ZENSzKyWoc5sMTC6oZ3dzYO/O5wH9+wXAAuH9e1PCJuz5HtmzXwSmKGzinudhk/asD5+wZ37YxL2XwyfueR8+abcF1CUsQPnHERP3loAVB/ft2h/cv2s+rrABaQjq3XUZiOFqAnt3LQbi42COb9f2575d20rQNXl373` &&
                 `AByiWA/OvTuX25T9f2/TB5IL8BGBb3IaHftvU/KMRhku7tO1TcWrdeB+L7rm1b9gNj5zYQ/wexYWrcWjc3gGIEYlvjps8OTRtzELZv6geK3XZo2AwPE/uGTfPtmzbtR/A3LwfyN4M5VrUbrlvWbpgOk7SqXTfbqm79aYSB8zmA/MOWtevBBljWb1SxrFn/2bJmn` &&
                 `Q9YgUnVmunGlWvvg/wN4htXrgkwqVz736Ry9Wogu9u4Ys11MC5f896kYnUDkH3fpHw1PMoZdIvXaOiVrvquV7oSHpA6JasddEtWTQZhEFs7axWPbvHqGt3iVfP1SlZHwCyDA/XC5RkaBcv/qxcsK8CQJBYo5i4JUcxZ+l4xZ8luxdzFOUpZy2wUCpYrgLBy3iId` &&
                 `heylEUD5yQq5SxNwGiKVtkxEOnNRjVTmou3S6YveS2cs+g/E34H4MZC/XypjUbt06nwDslyIDQAAQgkg3iT77dwAAAAASUVORK5CYII=`.

      WHEN c_image_keys-association.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABBElEQVR4nGNgoAYQaD+vINJ69qBI65l3oq2nj4L4JBkg1n72RPbWh397jz3/n7D+3l+x1tPHSDOg7cxH+wXX//suv/Xfbv71/6Itp74zhM78jxOjA/GOs6cNZl75bzr76n8QLdZ+5jhO27AZAPKz` &&
                 `aNvZ06JtZz6Jt509gzcMsBlAEiDbgNCZk1HCIWTWdNIMcGhgAWrcDjFg1n6GhPkcZLhiKg9D6IzNDJEzRUjWCwpYyc5zV5R6z/+Q7Dhzg5zEdjlry/3/fcdBie3uf9H2sxdIMgAYzV8s513777To+n8QDeR/INUFJwxmXv1jAk5sV/+ItZGY3OEZru3MO1BeITkM` &&
                 `cAEAZhGbMbBbTB8AAAAASUVORK5CYII=`.

      WHEN c_image_keys-extended_by.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABKklEQVR4nM2SIVMCURSFCQYjwbDBsIFgIBAIBgKBYCAYDAaCP8BAIBic2TdDIBA2EIgGAtFo2EAwGglGAsFgIBgIBDxn5nPGebMMzXFnvnm897jnnnN3K5V/94QQGuJJrMRefIkX0RUnx4ofxEaM` &&
                 `RN0FIsmy7FbrG0JnpcX6070u11prWlORi0LMdXZDg6lYlHU+d2eKbfVDDLVvsl9YSJzaic57sYAtT8QFxV6vxVj0ieK53HFexAJL0cJ2oPOGmbzajWgTqerBxgI77HlIVzgacBfIXieK3exjgS0CBZkf3Z07d08QynH3HgskKI+J0SBCn9+OsubtWDA/9B2kDLFF` &&
                 `4TPzmRHh5w2lpQKIdOjuYdZ+OXSsT51dHiyOnEyx7c95xT45WvznzzfhJAGIuot7RgAAAABJRU5ErkJggg==`.

      WHEN c_image_keys-from.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABQklEQVR4nK3Su0oDQRTG8TyCoK3EKpdFSVC8ZCEWUVDQyifwAXwGGxUbUXTWXEw2BhMt0lnYimijGEz2kt1EMSgqbqGtiKt8zskDHBAcGIY5f+ZXTSDw3ytVuAmqxeaFqjufiaIDVW46E7rzJWfX` &&
                 `1FlAPvaTJQczlTaWTh4gLj0sVO8we9jGZMmFRH5YYEJ3MFV2MX90i+XTJ6SvPCwed7r36YMWqLPAeKEJRTOQ3HcRz1oICwOxjImxvI2onFNngdF8E71rNcyVW6ha77BfP7B+/tIFaU6dBUb2bBagzgLDOR6gzgLxLA9QZ4FYxmIB6iwwlOYB6iwwuGuyAHUWUDQ` &&
                 `eoM4CYWF2gpv175R8UGm8dYGVs2ckCg76N+p+RJgeCwxs1XvC29aq/I33ijB8+olR0fAjmvkYEuZOKFfrY4G/rl+7wFtBvoD5FwAAAABJRU5ErkJggg==`.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
