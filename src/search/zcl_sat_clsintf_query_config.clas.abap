"! <p class="shorttext synchronized">Configuration for Class/Interface Query</p>
CLASS zcl_sat_clsintf_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.

    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    ALIASES c_custom_options FOR zif_sat_c_object_search~c_custom_options.

    CONSTANTS:
      BEGIN OF c_clif_image_keys,
        method TYPE string VALUE 'ABAP:IMG_METHOD',
      END OF c_clif_image_keys.

    METHODS build_config REDEFINITION.

    METHODS get_clif_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        folder    TYPE string VALUE 'ABAP:IMG_FOLDER',
        abap_lang TYPE string VALUE 'ABAP:IMG_SOURCE_CODE',
        friend    TYPE string VALUE 'ABAP:IMG_FRIEND',
        interface TYPE string VALUE 'ABAP:IMG_INTERFACE',
        super     TYPE string VALUE 'ABAP:IMG_SUPER_TYPE',
        attribute TYPE string VALUE 'ABAP:IMG_ATTRIBUTE',
      END OF c_image_keys.

    METHODS get_clif_type_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_flag_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_category_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_method_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_interface_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_attribute_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_friend_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_super_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_clsintf_query_config IMPLEMENTATION.
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
                                                                                ( get_description_filt_conf( ) )
                                                                                ( get_max_rows_filt_conf( ) )
                                                                                ( get_clif_type_filt_conf( ) )
                                                                                ( get_flag_filter( ) )
                                                                                ( get_category_filter( ) )
                                                                                ( get_method_filter( ) )
                                                                                ( get_interface_filter( ) )
                                                                                ( get_attribute_filter( ) )
                                                                                ( get_friend_filter( ) )
                                                                                ( get_super_type_filter( ) ) ).

    ms_search_type = VALUE #(
        label    = 'Class/Interface'
        name     = zif_sat_c_object_search=>c_search_type-class_interface
        img_info = VALUE #(
            img_key     = c_type_image_key_prefix && to_upper( zif_sat_c_object_search=>c_search_type-class_interface )
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAJVSURBVDhPpVLPTxNBFH6z2112mrbUtIAVlKQo` &&
                          `KBaDDYqQYACDHIyEiDQkJlr+BvSIBiXRizdJTIzBkwcaT4qebC8e0EMrVqKQGjgoUksJwrbd7f4YdyZKPHS9+CUzyfvme997+94iQgj8D5gBmuqDBoW0ebDjpheLFwEB2ilp85uyetuQ+cUfMwnbKg56` &&
                          `0eQal/j6StdhfyhwCokODKmvb4afLmR6sqh03pKkqK4SOHo5RX5yMFTvHwtfR0HfaTjobYOx8AQaaA343FicZEobsA5Enh+qczUjp1AN0/O32AMWMRRUFQkcusAIG7AOVM2A7M8CVAkYtmQDNnc1OFIT` &&
                          `ho1tBRTNQExpA2ZQ1IznC18+kJVsBu6N3IXp4WnoDHbBjqKTkma+ZEobsC3U3TgbwqIj3rzf6w8dOIo8khverSbJp418viU9+KBGb+iUMN+ulPSkqugPY/HxFzSZbfDPGusK5okqgZ8UeTSErDWWrcon` &&
                          `kxFB1LG740xjjyQJrOLy56yZW5cjc4nosz2DSoice9KKJUe6scnHXb7WDsfaaiHxKgOpt9/g4+J6US3px+fi0TU2Axu0ePZhTivrsLqyCYLIw9L777C9VYDqasmJONRERbYGCKGyvKNAPieDvKswjibT` &&
                          `eNfiCRAn5WwNTGIul4q6bhgmdHQfYlx3fxBorKq6YX15mnK2M7A6gNG+2QnsFO7UBlyY5znQrP8ll7X6KepTscT4/X8OkRpQjPQ/HuAQN2v1XG9RS8REV2OJaJK+sVx6VTp/Y7R3Bl3qfYR/h3sghMAv` &&
                          `aigRpe8kX2AAAAAASUVORK5CYII=` )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.

    ms_search_type-custom_options = VALUE #(
        ( key         = c_custom_options-mode_for_intf_super_filter-name
          type        = zif_sat_c_object_search=>c_custom_option_data_type-combo
          label       = 'Mode for &Hierarchy Filters:'
          description = |Controls whether the search will resolve the full type hierarchy of either\n| &&
                        |  • a given interface in the 'intf' filter or\n| &&
                        |  • a given class in the 'super' filter.\n\n| &&
                        |Note: The hierarchy will only be resolved when a single value is supplied for either filter (Wildcards/Negation not allowed)|
          values      = VALUE #( ( key = space value = 'No Hierarchy Resolution' )
                                 ( key   = c_custom_options-mode_for_intf_super_filter-options-resolve_intf
                                   value = 'Resolve Type Hierarchy for ''intf'' Filter' )
                                 ( key   = c_custom_options-mode_for_intf_super_filter-options-resolve_super
                                   value = 'Resolve Type Hierarchy for ''super'' Filter' ) ) ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-class_interface.
  ENDMETHOD.

  METHOD get_clif_type_filt_conf.
    result = VALUE #(
        name             = c_general_options-type
        description      = 'Object Type'
        long_description = |Use '{ c_general_options-type }' to restrict the search query to Classes and/or Interfaces|
        img_info         = VALUE #( img_key     = c_general_image_keys-type_folder
                                    img_encoded = get_general_image( c_general_image_keys-type_folder ) )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            caching         = abap_true
            category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_type
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) ).
  ENDMETHOD.

  METHOD get_flag_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-flag
        description      = 'Flag'
        long_description = |Use '{ c_class_intf_search_option-flag }' to search for Classes/Interfaces that have certain criteria.\n\n| &&
                           |Example:\n   { c_class_intf_search_option-flag } : abstract|
        img_info         = VALUE #( img_key     = c_general_image_keys-checked_box
                                    img_encoded = get_general_image( c_general_image_keys-checked_box ) )
        content_assist   = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    caching         = abap_true
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_flag ) ).
  ENDMETHOD.

  METHOD get_category_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-category
        description      = 'Class Category'
        long_description = |Use '{ c_class_intf_search_option-category }' to restrict search result to Classes/Interfaces that have a certain category.\n\n| &&
                           |Example:\n   { c_class_intf_search_option-category } : exception|
        img_info         = VALUE #(
            img_key     = c_image_keys-folder
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAVFBMVEUAAAD48Mj46LDo0Ijw2JD44Jj44KD42Ij40HjgwHj42JD40IDYsHC8hTLDiza0fzKlbCStciu8fzLDhTatbCSeZietci+eXx2e` &&
                          `XyCPUhn////46JgjdYhrAAAAAXRSTlMAQObYZgAAAFlJREFUeJydz0kSgCAMRNEoKojiBMaB+9/TEFPo0vLtfu8a4JeBvDucJDztY6xU2/uEB1dkjoelzHYexoZgBjBba7EWioZJa9QiNWwGO8ENBxpx` &&
                          `N8CafTt2AcOvBf0oDmf4AAAAAElFTkSuQmCC` )
        content_assist   = VALUE #(
            assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            caching         = abap_true
            category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_category
            proposal_images = VALUE #( ( img_key     = c_general_image_keys-type_group
                                         img_encoded = get_general_image( c_general_image_keys-type_group ) ) ) ) ).
  ENDMETHOD.

  METHOD get_method_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-method
        description      = 'Method'
        long_description = |Use '{ c_class_intf_search_option-method }' to search for Classes/Interfaces that have certain methods.\n\n| &&
                           |Example:\n   { c_class_intf_search_option-method } : set_value|
        img_info         = VALUE #( img_key     = c_clif_image_keys-method
                                    img_encoded = get_clif_image( c_clif_image_keys-method ) )
        allowed_length   = 61
        patterns         = abap_true ).
  ENDMETHOD.

  METHOD get_interface_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-interface
        description      = 'Interface Usage'
        long_description = |Use '{ c_class_intf_search_option-interface }' to search for Classes/Interfaces that implement or extend certain interfaces.\n\n| &&
                           |Example:\n   { c_class_intf_search_option-interface } : if_salv_adapter|
        img_info         = VALUE #( img_key      = zif_sat_c_object_types=>interface
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #( assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris
                                    adt_object_types = VALUE #( ( |{ zif_sat_c_tadir_types=>interface }| ) ) ) ).
  ENDMETHOD.

  METHOD get_attribute_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-attribute
        description      = 'Attribute'
        long_description = |Use '{ c_class_intf_search_option-attribute }' to search for Classes/Interfaces that have certain attributes.\n| &&
                           |This parameter also allows the input with Key/Value pattern.\n\nExample:\n   { c_class_intf_search_option-attribute } : mv_plant\n\n| &&
                           |Example with Key/Value:\n   { c_class_intf_search_option-attribute } : c_*=*constant|
        img_info         = VALUE #(
            img_key     = c_image_keys-attribute
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsIAAA7CARUoSoAAAAEbSURBVDhPY/z//z8DJYAJSpMN4C44o29FklNM` &&
                          `Lh5jBNEoBij6u4HZMMDIxcXw//t3BgY0b97fuAtuAG4vMDIyCFWUMQjVVEEFsAOsBrA/e8PALacI4bCwMHCJSoLFsAGcLmCPiWD4feQ4w79bdxg4kmPBLsIGsBrA6u7MwMjOxvD/02eGP2fPg8OCxdIM` &&
                          `KosKMA0A2sQW5Admsnq4MLBFhEDYDrYMzOLiYDYywDCAy9ODgZGZmeH31p0MXyxcwBgEmMTFGFikJMFsZIBhAKeFOcP/P38Yfk6aCRVhYPi1ZiMDIzcXA5ebK1QEATDTASsr2AX/f/wAi4MBMCbAAGgw` &&
                          `COBPB79/o2oGAZBGqGZ0QL2kTC7Amg6IBwwMAHM6YAiWs+qtAAAAAElFTkSuQmCC` )
        allowed_length   = 30
        key_value        = abap_true
        patterns         = abap_true ).
  ENDMETHOD.

  METHOD get_friend_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-friend
        description      = 'Global Friend'
        long_description = |Use '{ c_class_intf_search_option-friend }' to search for classes that have certain Global Friends.\n\n| &&
                           |Example:\n   { c_class_intf_search_option-friend } : if_alv_rm_grid_friend|
        img_info         = VALUE #(
            img_key     = c_image_keys-friend
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACYUlEQVR4nM2S31MSURTHffCxh/6JHvsfeuCh1MapMbFJCwFLa3oxZyBsGjORSvkxkakYQQlL8sNJS6VCFhWacSwde21MSHQBd9nLsrDL` &&
                          `Lrvbro0zkYwPPXVmvnPu+d5zPvfhnpqa/y6WXeqm2NSt4LL7Or3k6hBWoBtE1Nv1CXZ3yI4dhOG+2sikajY61YnF140Yte/b44iZZAGBkO21YbDi6czDr9VB2HvzRFVA2KFwrs12Z9ns9G5ZFIe/TUqA` &&
                          `A4lnWgR+Ee/Dr5QzR4bnRuUNEbcKJTeeY3TCnQKhJwT2Xk9Kg9l5PSnV7K4HIdZHsfBkO5gfa2mqACzYLkd2Ns3YPqSjM29+K+3SUNLriPNO8dCX8s6mBRP7YxWAGevFfDHtSme8WjLxqK3040FzKePX` &&
                          `5jjCj6S9PbnvukbRk5fSUA9JZdzpWesFugLgG64TOMKzLwkL9ua2B+UUgzhRqWbRSTTx+AqFvtMQZQAd9PhN9cJfgHMgn7TiXPYhwYOBHA+GAA8sOAdGcB48Ff1hwOGGHI8P5PJ7JuAz1hUqAAFLw9zW` &&
                          `6r08l71Ptg5ZWZ3dzKyu64vxrf6ClKW6yfCszIv38a+9hYC5/mMlwNwom59oxhlssEhleinNhJE9fXuMP9n6QjjVNc53j5vKks+ihsIHuxybNp5vOPKVQVuLPepVkSw2SPG4luZxzR/S0pIfC6jzCza5` &&
                          `p+oiwX1nakOOq/YlSJn9uXG3VEwNMBzQM1Sqn9n7pmOXoHY85GjzwCOXqm/iYYReKmURlzK86FAIi45rQkgU7FJ+DjkUZ48d/Nf4BdpeseqJ1iPyAAAAAElFTkSuQmCC` )
        allowed_length   = 30
        patterns         = abap_true ).
  ENDMETHOD.

  METHOD get_super_type_filter.
    result = VALUE #(
        name             = c_class_intf_search_option-super_type
        description      = 'Super Type'
        long_description = |Use '{ c_class_intf_search_option-super_type }' to search for Classes with a certain Super Class.\n\n| &&
                           |Example:\n   { c_class_intf_search_option-super_type } : cl_gui_object|
        img_info         = VALUE #(
            img_key     = c_image_keys-super
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABYElEQVR4nGNgoCcISEgQ8I2Kne/g4MBCsmaQJp+o2N1AA/6DDMFQcGy5x9sjS53/H1/p9QibAVGpOdOB+D8CZ5ejKABp/vf3/X8QnV/W` &&
                          `ch+MS1s+wuQzCioU0vKrbdILqh6D2Cm5DTIoBqzvUvq2eYLe+w3d6h8KKtoVQDi/rPkxshqQWG55x32sftzcoyayvl1BYX2/ggBMDOQKZDUZFf0KmRUTsBuALQwwDZiukFo5E7sB2MIgr6z5O7KahIr5` &&
                          `CglVi7AbQEwYJFQsV4iuWkV+GDg07GeJLlsrg6kbB0A2QKjMQUa20nGKXKXjI5lyh26BAgcBfHoxDFCodFwRODX2d++O2v8eE8N/ggwhwoDm37AAlSi1+1uyIvd/5er8/5mL0v9Ll9k/JWhAblmLBSxA` &&
                          `ZUod9uk3e/x36Q38r9Po9lu8xH4WQQOQgWihpYpUhf1OsRLbH5Ll9qtBYQISBwDP0MXt5h1aTgAAAABJRU5ErkJggg==` )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #( assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris
                                    adt_object_types = VALUE #( ( |{ zif_sat_c_tadir_types=>class }| ) ) ) ).
  ENDMETHOD.

  METHOD get_clif_image.
    CASE iv_image_key.

      WHEN c_clif_image_keys-method.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAJbSURBVDhPxZNPSBRRHMe/772ZXWdczNRqKRdTtjKwP5bWE` &&
                 `gpB0dHoUkR46hBCxz15Cgo8GHSLSi95KSm1TnUpygzKhUKIwLCoiBRX/DM7szOzO/Neb2YH7eCtQ5/D/B5f5vt9M7/3e/hXSFTXqc92na5itN8XotPzRUKhxKaM5MouH8jfmnoevbbORsC1k0rKEXcJJRe69jRorc` &&
                 `ktdIuuwnQ9zM4bmPySt1zPm/AKrC9/+5UZucCiilSmaWh7Tfzi5e60vj/ZSFK1+7BV2406LYka3cPBlB77veKkHVFqNaZ+PI5slYDgs1WF3RjuzeptyQwM28H86gqWLUOui+hO9yC97RA4vqozv9aaYx2NH4rvfs4` &&
                 `FXho84pT2H95VqzGoaNuZwfmjV/Dy8wwmZz+hqe4A2lPH0N54Qu6m4XhzfbUSp/2BLyAM8LjorNerqMr0UHw4PYYnV0fl7jbOHenB6PR4qC+sGUjEFHBfdISCJAzgQiTKPkfJE6E4NDmCB+/H8CL7FI9y42C00utl` &&
                 `swi3zOFzoYWCJAwghJiLhiv/uRCKAcNvRnDq5lnceX0fQXjAiuVhqeBC5tmhIKkEANMLBZsb9vrpbErJ48ibrnSRXCRVAnyIgbwpWx8RY2q0qkBJNC6yzBdsi3tisCL8NUg7st33tLh6qaUhoSuUoVqtll1nUJiCN` &&
                 `XcVJe7h21KhaLnu+OLg297ItjFI1pmWZ1WO35y3nL1xRVEI9QlnZZheEatOCXOLhpzE8oSw1L5i7nspsm1+FxQ5F4LzDBfQgoYRkI/y6K5vdhf+N8AfNmv0xpyFHKIAAAAASUVORK5CYII=`.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
