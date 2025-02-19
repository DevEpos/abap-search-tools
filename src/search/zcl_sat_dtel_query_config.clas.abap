"! <p class="shorttext synchronized">Query Config for DTEL search</p>
CLASS zcl_sat_dtel_query_config DEFINITION
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
        label     TYPE string VALUE 'ABAP:IMG_FIELD_LABEL',
        ref_type  TYPE string VALUE 'ABAP:IMG_DTEL_REF_TYPE',
        component TYPE string VALUE 'ABAP:IMG_DEFAULT_COMP',
      END OF c_image_keys.

    METHODS get_label_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_length_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_table_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_dtype_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_type_category_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_ref_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_param_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_default_comp_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_flag_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_shlp_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_shlp_param_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_dtel_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-data_element.
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
        ( get_rel_state_filt_conf( iv_object_filter = zif_sat_c_object_types=>data_element ) )
        ( get_description_filt_conf( ) )
        ( get_label_filter( ) )
        ( get_flag_filter( ) )
        ( get_type_category_filter( ) )
        ( get_type_filter( ) )
        ( get_ref_type_filter( ) )
        ( get_dtype_filter( ) )
        ( get_length_filter( ) )
        ( get_table_filter( ) )
        ( get_param_filter( ) )
        ( get_shlp_filter( ) )
        ( get_shlp_param_filter( ) )
        ( get_default_comp_filter( ) )
        ( get_max_rows_filt_conf( ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_search_type_config(
                               label    = 'Data Element'
                               name     = zif_sat_c_object_search=>c_search_type-data_element
                               img_info = VALUE #(
                                   img_key      = zif_sat_c_object_types=>data_element
                                   img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
                               inputs   = VALUE #(
                                   ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                     label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                                   ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                                     label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                                     filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD get_label_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-label
        description      = 'Label'
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-label }' to search for data elements with a specific label (small, medium, long or heading).\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-label } : some+label|
        patterns         = abap_true
        img_info         = VALUE #(
            img_key     = c_image_keys-label
            img_encoded = |R0lGODlhEAAQAPf/APf4/ff4/PX3/PH0+/T3/ff5/TFOgm15j9La6dLa6PH1/Ojv+urw+u7z+/T3/M/Z6Ojv+fH1+/f5/HN9i87Z6O3z++7z+ujw+ufv+erx+ujw+erx+e3z+nuBhnuBhdXn44OGgIyM| &&
                          |eoyMe5SRdZWRdJ2WbqecaKOaa52Vb////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA| &&
                          |AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA| &&
                          |AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA| &&
                          |AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA| &&
                          |AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA| &&
                          |AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAACkALAAAAAAQABAA| &&
                          |AAiKAFMIHEiwoMGDCBMSNMGwoUOHAk18mEix4kQTAk98wMCxo0cMJwSW+ABBgwYICy6YXKBhAQqBIz5koGBgwwMDM3EyICFQxIcGCAxwCFohgQELFUIIBPEhglOnA54qiABCoIcPDrI6ELCVQFYCHQRO| &&
                          |+AAgQAAJANICKFB2gsADcOPKlauwrt27AwMCADs=| )
        allowed_length   = 40 ).
  ENDMETHOD.

  METHOD get_length_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-length
        description      = 'Length'
        range            = abap_true
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-length }' to search for data elements with a specific length.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-length } : 10|
        img_info         = VALUE #( img_key     = c_general_image_keys-length
                                    img_encoded = get_general_image( c_general_image_keys-length ) ) ).
  ENDMETHOD.

  METHOD get_table_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-table
        description      = 'Usages in Tables'
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-table }' to search for data elements that are used in specific tables (i.e. in table fields).\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-table } : mara|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>table_definition )
        content_assist   = VALUE #( adt_object_types = VALUE #( ( zif_sat_c_object_types=>table_type )
                                                                ( zif_sat_c_object_types=>structure ) )
                                    assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris ) ).
  ENDMETHOD.

  METHOD get_dtype_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-data_type
        description      = 'Data type in ABAP Dictionary'
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-data_type }' to search for data elements that use a specific ABAP DDIC type.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-data_type } : char|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>type_group )
        content_assist   = VALUE #(
            caching               = abap_true
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-builtin_data_type
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_type_category_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-type_category
        description      = 'Category'
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-type_category }' to search for data elements of a certain category.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-type_category } : predefined|
        img_info         = VALUE #( img_key     = c_general_image_keys-folder
                                    img_encoded = get_general_image( iv_image_key = c_general_image_keys-folder ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) )
            proposal_values = VALUE #(
                ( name = zif_sat_c_os_dtel_options=>c_type_category-ext-domain description = 'Domain Type' )
                ( name = zif_sat_c_os_dtel_options=>c_type_category-ext-ref_type description = 'Reference Type' )
                ( name = zif_sat_c_os_dtel_options=>c_type_category-ext-predefined description = 'Predefined Type' ) ) ) ).
  ENDMETHOD.

  METHOD get_type_filter.
    result = VALUE #(
        name             = zif_sat_c_object_search=>c_general_search_params-type
        description      = 'Type Name'
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_object_search=>c_general_search_params-type }' to search for data elements that are using a certain type (i.e. domain, class, ...).\n\n| &&
                           |Example:\n   { zif_sat_c_object_search=>c_general_search_params-type } : ddtext|
        img_info         = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
        content_assist   = VALUE #( adt_object_types = VALUE #( ( zif_sat_c_object_types=>table_type )
                                                                ( zif_sat_c_object_types=>structure )
                                                                ( zif_sat_c_object_types=>class )
                                                                ( zif_sat_c_object_types=>interface )
                                                                ( zif_sat_c_object_types=>domain ) )
                                    assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris ) ).
  ENDMETHOD.

  METHOD get_ref_type_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-ref_type
        description      = 'Reference Type'
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-ref_type }' to search for data elements with a certain reference type category.\n\n| &&
                           |Example: \n   { zif_sat_c_os_dtel_options=>c_filter_key-ref_type } : class|
        img_info         = VALUE #(
            img_key     = c_image_keys-ref_type
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACC0lEQVR4nGNgoBYQKbGNkKtyviVRZvdTvsL5oVSpQziyfLjbFJ9ov1m3Iryn/4zxm/kw0ntaPFxSsswhWrHG5WvX9ub/m87N/t+/q+0/| &&
                          |iC9R7ABWFO4xJSIuYPaXOZOO/d+8+vr/+VNP/gfx4YbIVznfrV1f+X/B4T44btxY81+2wukpSD7ad+atad2H/q9efAmOZ/Yf/R/jO/MJ2ACJUtvvBcvz/qNj0RLb3yD5CM/p31srd/6H4bYqCA7znAqW| &&
                          |ZxAvtbtl0uzx37HLF45NgXygwQ/BXvCcdivOb+7/hIAFcBznNw9owDSwPINooY2HWInNN5N2r7+O/f7/TTu8/osBXSVeYusPkg91n+oBNORjcuiSPxkRK/6nhCz+H+41/VOU+xR/eEAKFVlbAGNgHxA/| &&
                          |Ey+zOwrig8QNEvoFwIZ4TTWI8p6+O9JrxrNI7+lHw9ynueCNVoPUaW36KdO+G2XO/Axkf9BPnpREdJowSJqSZpE/70vG+gv/i/bc+p+29ux/k6xZn3WTJuG3FQYM02c+Tl5x5n/BzptwHDP/yH+jjJmn| &&
                          |iTJAN3HyX9+Off+RsWfzjv/6ydNeE2dA0tSXllUb/9s174Fji7I1//WSp54j0oBJLrrJU79b9uz8bz15/3/z1k3/dZOnfNZJmmxClAEgoJcwycIgbfpB/YwZJ/XTZ8wHRqcC0ZpJAQAOtSP9pj5DxwAA| &&
                          |AABJRU5ErkJggg==| )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) )
            proposal_values = VALUE #(
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-built_in description = 'Built-in DDIC Type' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-data_element description = 'Data Element' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-data description = 'Data' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-object description = 'Object' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-class description = 'Class' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-interface description = 'Interface' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-table_type description = 'Table Type' )
                ( name = zif_sat_c_os_dtel_options=>c_ref_type-ext-structured description = 'Structured' ) ) ) ).
  ENDMETHOD.

  METHOD get_param_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-param
        description      = 'SET/GET Parameter'
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-param }' to search for data elements with a given SET/GET Parameter ID assigned.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-param } : matnr|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>set_get_param )
        content_assist   = VALUE #( adt_object_types = VALUE #( ( zif_sat_c_object_types=>set_get_param ) )
                                    assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris ) ).
  ENDMETHOD.

  METHOD get_default_comp_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-default_component
        description      = 'Default Component Name'
        patterns         = abap_true
        allowed_length   = 30
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-default_component }' to search for data elements with a given default component name.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-default_component } : bukrs|
        img_info         = VALUE #(
            img_key     = c_image_keys-component
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAdhAAAHYQGVw7i2AAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAR5JREFUOI2tkiFPw1AUhb9bGvo| &&
                          |GQfED9gtIEAyBwyHQCAQ0QZIQBBKLmp1cQodBgMGARBKC2z8Yv4CQAF0ZvYj1dW3ahdeEa17efeece07uE2pKr1jmx+zjoUh8LQd81OEApEJ+xGdknoCNrPVCO96SbSZ1Al6l87q0ViADdLLefAd6aS| &&
                          |4QzoDFecCsEqArYXxeFojM2IGci0gYB/ZiI7iSK1grMG4ggEbBifZNG2yEQbCLejugR4BxlHljIuulNWrU6oEeO1sRPc3XqIOVVdCOMxkgXRj508nmGf3ebEB9R7VP+Hln16hNBksY59GrPzFHcQN6+| &&
                          |5eYn50Jpf3KUA6/9qbuWkPQ4ldOigLWQbfwkCJ6P4OkD4CNmGTY/6tf0vZTXLEwE0wAAAAASUVORK5CYII=| ) ).
  ENDMETHOD.

  METHOD get_flag_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-flag
        description      = 'Flag'
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-flag }' to search for data elements with certain criteria.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-flag } : input_history|
        img_info         = VALUE #( img_key     = c_general_image_keys-checked_box
                                    img_encoded = get_general_image( c_general_image_keys-checked_box ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = VALUE #(
                ( name = zif_sat_c_os_dtel_options=>c_flag-changedoc_enabled description = 'Change Document Logging' )
                ( name = zif_sat_c_os_dtel_options=>c_flag-input_history     description = 'Input History in Dynpro Fields' )
                ( name = zif_sat_c_os_dtel_options=>c_flag-basic_dir_is_ltr  description = 'Basic Direction is Left-to-Right' )
                ( name = zif_sat_c_os_dtel_options=>c_flag-bidi_filtering    description = 'Filtering of BIDI Formatting Characters' ) ) ) ).
  ENDMETHOD.

  METHOD get_shlp_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-shlp_name
        description      = 'Assigned Search Help'
        allowed_length   = 30
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-shlp_name }' to search for data elements with a given search help assigned.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-shlp_name } : tabl|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>search_help )
        content_assist   = VALUE #( adt_object_types = VALUE #( ( zif_sat_c_object_types=>search_help ) )
                                    assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris ) ).
  ENDMETHOD.

  METHOD get_shlp_param_filter.
    result = VALUE #(
        name             = zif_sat_c_os_dtel_options=>c_filter_key-shlp_param
        description      = 'Assigned Search Help Parameter'
        allowed_length   = 30
        patterns         = abap_true
        long_description = |Use '{ zif_sat_c_os_dtel_options=>c_filter_key-shlp_param }' to search for data elements with a given search help parameter assigned.\n\n| &&
                           |Example:\n   { zif_sat_c_os_dtel_options=>c_filter_key-shlp_param } : tabname|
        img_info         = VALUE #( img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type
                                    img_key      = zif_sat_c_object_types=>search_help ) ).
  ENDMETHOD.
ENDCLASS.
