"! <p class="shorttext synchronized">Configuration for messages Query</p>
CLASS zcl_sat_message_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_os_mess_options.

    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type          REDEFINITION.
    METHODS zif_sat_object_search_config~get_output_config REDEFINITION.
    METHODS zif_sat_object_search_config~get_option_config REDEFINITION.
    METHODS zif_sat_object_search_config~has_option        REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_msg_search_params FOR zif_sat_c_os_mess_options~c_filter_key.

    DATA mt_message_options TYPE zif_sat_ty_object_search=>ty_query_filters.

    METHODS get_self_expl_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_message_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-message.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_output_config.
    result = super->zif_sat_object_search_config~get_output_config( ).
    result-adt_type_img_map_entries = VALUE #(
        ( adt_type   = zif_sat_c_object_types=>message
          image_info = VALUE #(
              img_key     = 'ABAP:IMG_ADT_TYPE__MESSAGE'
              img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAACFSURBVDhP3ZFLDoAgDETB30oP5Xk8j+fxVu| &&
                            |6kzmAxhIV2QWLiS5p2oKVtcD9iXsXTVJq5C1DcwMm2eFHNu54x4FluHnkHfEXQUWgqzeQrxGKMFs/eHkt59UDHETapNJOv0MEFjBZUP46Yfqse6NjSVJopVzjSaMUKjNPjvB+Qt1/yW5w7AcJ5OMzr| &&
                            |dIT0AAAAAElFTkSuQmCC| ) ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_option_config.
    IF iv_target = zif_sat_c_os_mess_options=>c_search_fields-message_filter_input_key.
      rs_option = mt_message_options[ name = iv_option ].
    ELSE.
      rs_option = super->zif_sat_object_search_config~get_option_config( iv_option ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~has_option.
    IF iv_target = zif_sat_c_os_mess_options=>c_search_fields-message_filter_input_key.
      rf_has_option = xsdbool( line_exists( mt_message_options[ name = iv_option ] ) ).
    ELSE.
      rf_has_option = super->zif_sat_object_search_config~has_option( iv_option = iv_option iv_target = iv_target ).
    ENDIF.
  ENDMETHOD.

  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_query_filters( ( get_user_filt_conf( ) )
                                                                                ( get_created_on_filt_conf( ) )
                                                                                ( get_changed_by_filt_conf( ) )
                                                                                ( get_changed_on_filt_conf( ) )
                                                                                ( get_package_filt_conf( ) )
                                                                                ( get_softw_comp_filt_conf( ) )
                                                                                ( get_appl_comp_filt_conf( ) )
                                                                                ( get_rel_state_filt_conf( ) )
                                                                                ( get_description_filt_conf( ) )
                                                                                ( get_max_rows_filt_conf( )  ) ).

    mt_message_options = VALUE #( ( get_changed_by_filt_conf( ) )
                                  ( get_changed_on_filt_conf( ) )
                                  ( get_self_expl_filt_conf( ) ) ).

    ms_search_type = VALUE #(
        label    = 'Message'
        name     = zif_sat_c_object_search=>c_search_type-message
        img_info = VALUE #( img_key      = zif_sat_c_object_types=>message_class
                            img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters )
                            ( name    = zif_sat_c_os_mess_options=>c_search_fields-message_text_input_key
                              label   = zif_sat_c_os_mess_options=>c_search_fields-message_text_input_label )
                            ( name    = zif_sat_c_os_mess_options=>c_search_fields-message_filter_input_key
                              label   = zif_sat_c_os_mess_options=>c_search_fields-message_filter_input_label
                              filters = mt_message_options ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD get_self_expl_filt_conf.
    result = VALUE #(
        name             = c_msg_search_params-self_explanatory
        description      = 'Is Self Explanatory?'
        long_description = |Use '{ c_msg_search_params-self_explanatory }' to restrict query to messages that are self explanatory.\n\n| &&
                           |Example:\n   { c_msg_search_params-self_explanatory } : true|
        data_type        = zif_sat_c_object_search=>c_filter_data_type-boolean
        single           = abap_true
        img_info         = VALUE #(
            img_key     = 'ABAP:IMG_MSG_SELF_EXPL'
            img_encoded = |iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACm0lEQVR4nJ2T3U+ScRTH/QO66LLLLlu18oWFL1ludWEmZRCkVtOabuZyunLLXDUNE5CAB0ymS3PafEvtRUNBLVKH5ts0rQmaKBoiIr6B| &&
                          |8qTRt4enxrK6oe92Ls7Z73x2zu+c4+f3S6lF2j1pSu2jBEGz6eztpw8iMmS7/XxRurKzVljVu13bNobE3AYyKL5A7BMgTdlJdg/NYPCTGfmlWjDiRWYfAe80hQ39eNM3hSR+vZtxUVjmHynYe+yC4lk4| &&
                          |T27yP12AUA4xzzwnKz7Azdn1F+CKqPn85bwXs9GZFd+YCVLjkVhCyGRLnUXVPdsjBgsGxs0Y1ltwl1C7Qjgya0CUiOFNTs57xU8WNDlzS98iU94KXlaNK5gjw5hxEZqeCYTzCARECRERp8DwhAWqLj2C| &&
                          |2bIFbyWpwtfO8pcD0PZP4XFDHyKvlqCssR+OLTc418oQl14By/IGLt2oRAa/kY5nipqczBgp/+cExCp09E5ilKLXtIwgkCXGrG0da+Q2FlY2sLjugn7WjuOxCtwvVNNxtY6qLFbRRwNuSlrQNWiEYcaG| &&
                          |+rZRUD1ixbW1w45yCaTcqYN1bZP2x01LCOUSNhqQRajRRvU6op9HtWqY6k+K6YVVLG98pc1APT4Rr8DnL8veWEu3gYLKTTQgm2g9lChohIvcQn7NAKJTnqCkrhdLDpK28Wkb5BVdsK5uemNZYpXnoyXe| &&
                          |SfByauFRHgVIV3SAyZZ9H6JGt0iVrOkxgHe9HJNzdtp/3j7mmYJ1xz78Dsgu1+EwR7IewiEcRVU69/uPczDbHdB9MOGevJWkku2Bp0TBOxbpT8BBrsQWxHq4L5Qj1zDOSMgAlthNbaQxjEcU+8f849A8| &&
                          |ANatSpxMK0VYkhL72WLfbuF/9QNtD+HnUTyLEAAAAABJRU5ErkJggg==| ) ).
  ENDMETHOD.
ENDCLASS.
