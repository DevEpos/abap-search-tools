"! <p class="shorttext synchronized">Query Config for DOMA search</p>
CLASS zcl_sat_doma_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        decimals  TYPE string VALUE 'ABAP:IMG_DECIMALS',
        conv_exit TYPE string VALUE 'ABAP:IMG_CONV_EXIT',
        fix_value TYPE string VALUE 'ABAP:IMG_FIX_VALUE',
      END OF c_image_keys.

    METHODS get_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_flag_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_length_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_decimals_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_output_length_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_value_table_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_dtype_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_fix_value_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_conv_exit_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_doma_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
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
                                        ( get_rel_state_filt_conf( iv_object_filter = zif_sat_c_object_types=>domain ) )
                                        ( get_description_filt_conf( ) )
                                        ( get_flag_filter( ) )
                                        ( get_type_filter( ) )
                                        ( get_dtype_filter( ) )
                                        ( get_length_filter( ) )
                                        ( get_output_length_filter( ) )
                                        ( get_decimals_filter( ) )
                                        ( get_conv_exit_filter( ) )
                                        ( get_value_table_filter( ) )
                                        ( get_fix_value_filter( ) )
                                        ( get_max_rows_filt_conf( ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_search_type_config(
                               label    = 'Domain'
                               name     = zif_sat_c_object_search=>c_search_type-domain
                               img_info = VALUE #(
                                   img_key      = zif_sat_c_object_types=>domain
                                   img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
                               inputs   = VALUE #(
                                   ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                     label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                                   ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                     label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                                     filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-domain.
  ENDMETHOD.

  METHOD get_dtype_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-data_type
        description      = 'Data type in ABAP Dictionary'
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-data_type }' to search for domains that use a specific ABAP DDIC type.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-data_type } : char|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>type_group )
        content_assist   = VALUE #(
            caching               = abap_true
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-builtin_data_type
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_flag_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-flag
        description      = 'Flag'
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-flag }' to search for domains with certain criteria.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-flag } : lowercase|
        img_info         = VALUE #( img_key     = c_general_image_keys-checked_box
                                    img_encoded = get_general_image( c_general_image_keys-checked_box ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = VALUE #(
                ( name = zif_sat_c_os_doma_options=>c_flag-lowercase description = 'Case-sensitive' )
                ( name = zif_sat_c_os_doma_options=>c_flag-fix_values     description = 'Has Fixed Values' ) ) ) ).
  ENDMETHOD.

  METHOD get_length_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-length
        description      = 'Length'
        range            = abap_true
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-length }' to search for domains with a specific length.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-length } : 10|
        img_info         = VALUE #( img_key     = c_general_image_keys-length
                                    img_encoded = get_general_image( c_general_image_keys-length ) ) ).
  ENDMETHOD.

  METHOD get_output_length_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-outlength
        description      = 'Output Length'
        range            = abap_true
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-length }' to search for domains with a specific output length.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-length } : 10|
        img_info         = VALUE #( img_key     = c_general_image_keys-length
                                    img_encoded = get_general_image( c_general_image_keys-length ) ) ).
  ENDMETHOD.

  METHOD get_type_filter.
    result = VALUE #(
        name             = c_general_options-type
        description      = 'Type'
        long_description = |Use '{ c_general_options-type }' to restrict the search query to domains which are of a certain type.\n\n| &&
                           |Example:\n   { c_general_options-type } : domain|
        img_info         = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) )
            proposal_values = VALUE #(
                ( name = zif_sat_c_os_doma_options=>c_type-domain description = 'Domain' )
                ( name = zif_sat_c_os_doma_options=>c_type-append_domain description = 'APPEND Domain' ) ) ) ).
  ENDMETHOD.

  METHOD get_value_table_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-value_table
        description      = 'Value Table'
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-value_table }' to search for domains that have a certain value table.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-value_table } : mara|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>table_definition )
        content_assist   = VALUE #( adt_object_types = VALUE #( ( zif_sat_c_object_types=>table_definition ) )
                                    assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris ) ).
  ENDMETHOD.

  METHOD get_decimals_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-decimals
        description      = 'Decimals'
        range            = abap_true
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-decimals }' to search for domains with a specific number of decimals.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-decimals } : >3|
        img_info         = VALUE #(
            img_key     = c_image_keys-decimals
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAASVJREFUOI21kTFLw3AQxX+XaNFN| &&
                          |Cy66aKnfQYQKOjo0VekipnZ0st9BcdFFcBaEKoKdooM6FZqirsVBhC4uToJSCkWa/M9dmmhA3/h49+PdnRCh9TU/Mxxa2eDTfji7nutE5axBpltolO2AZ6Pm1k71n8qF+nQigKjsgRwFJphQ6ITGriQC| &&
                          |KEyien9+tfiG8IglU4kASfRvgC4W4wCipEXp/ggoOY0DN99cBRCoicq+6/h1hSWjXCRq0Eult1DdEaStyvLpZe7mV/v8uVzH33YdvxWXifzCRt6vAIfAcRxABpmlleamGj2JHtN21VuYBRgaWOu974Vj| &&
                          |dgsjM6pmV8R6+Vb7NbYBQLF4NzrSDz2UbNXLZaJykTeo1eZ79keQt4Ri9CrwBeUxZGSTvhgpAAAAAElFTkSuQmCC| ) ).
  ENDMETHOD.

  METHOD get_conv_exit_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-conv_exit
        description      = 'Conversion Exit'
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-conv_exit }' to search for domains with a specific conversion exit.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-conv_exit } : alpha|
        patterns         = abap_true
        img_info         = VALUE #(
            img_key     = c_image_keys-conv_exit
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAWQAAAFkBqp2phgAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAH2SURB| &&
                          |VDiNhZO9a5NRFMZ/532T2CSv+VLMEoiQDB0CIgWrJrXJIKIOooW41WqFouKm4uh/4CKoWKljyWRFyFCkAatQMYO0JA4VaQhRUYhNiWk+zHVoTPOB8Wz3nvt77nMeOKKUYlDJq7cJKuWzGI5TKjq63NvX| &&
                          |BsKZvIfqdoxq1Ubp55KkViJ9b3odSCZjQbffBnURJMTaqrSbZnMDhyvW6cTUBX/MjaDbnwEhEPqqXjdRKT8H9vcJSPazH01bBNyDxqLZHOo8agCSTpvR9MR/YV1XGLabfQLs3XcB1JHWXQHFPMINhDNY| &&
                          |rZtt2O2eUePH5wAOpy67OkaQcwAo7jPsv6WgOflt0h7wBir3NmJZarVRXO7ravzoEwB/6tL89xITgHlHQEkEmtfU8MFHAPHcdERp+sJqfuMBJ0+EeZn2qOjIj1Ambil+sb4rFDl0wEFu14FeC6lgsLQL| &&
                          |/06CGIKKBZan6nVDhbyLHNssW33Vxs7Y9j0stAX+whOFqbCmkQQxWnmMffqqxnqzdNooe7aG7nZk0Eq0KQ8BoxfoLJOO8jq10+/Dj3/1CQgUB22GYWXb55Qr2fDc67Zg9xN9GhpLIL5e0GWTN3aLfj4b| &&
                          |frrV9WnvLsTzV4OqLSLJlXXtTi46u/YvV33bmPDNrgummMAHJerFIBjgDyzXrBKzrb9dAAAAAElFTkSuQmCC| ) ).
  ENDMETHOD.

  METHOD get_fix_value_filter.
    result = VALUE #(
        name             = zif_sat_c_os_doma_options=>c_filter_key-fix_value
        description      = 'Fix Value'
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_os_doma_options=>c_filter_key-fix_value }' to search for domains that have certain fixed values.\n\n| &&
                           |Example:\n   { zif_sat_c_os_doma_options=>c_filter_key-fix_value } : prod|
        img_info         = VALUE #(
            img_key     = c_image_keys-fix_value
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABGElEQVR4nGNgGAVw4JrVb+CeOeG5Q0G/QEXHgv/VXYv+1/Ys+d/Qv/R/86QVYAxS557Zf98tY4IPqub0PhP37AnvM7s2fGiYf16gqGn2| &&
                          |/yUbDvxfvvnw/7U7T/zffujC/7reJWADCibteOGRPfEj3BCgiS4eWRM/ZHZv/Ny0+GQKSCynZtp/EABphIGK9vlgAxoWnnYpmbb3k2fWhNdgQ4AG/I9vWP6/YfHJ2TAXpZVNBGtGxkWNs//D5BsWnWjP| &&
                          |7tv8H6QX7oK8Cds/Niw5CXZWQkEvhguyga7C6gJYGAAFXuRN2PYaFAbROZ0YLkgFugoWBp7Zk95hBiQoFjL6H4NiAWQAugviC3pwxwI6ABmADZOYMkYMAAAK1e9tR2fCaQAAAABJRU5ErkJggg==| ) ).
  ENDMETHOD.
ENDCLASS.
