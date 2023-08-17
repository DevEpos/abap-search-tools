"! <p class="shorttext synchronized">Configuration for messages Query</p>
CLASS zcl_sat_message_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type          REDEFINITION.
    METHODS zif_sat_object_search_config~get_output_config REDEFINITION.
    METHODS zif_sat_object_search_config~get_option_config REDEFINITION.

  PROTECTED SECTION.
    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    DATA mt_message_options TYPE zif_sat_ty_object_search=>ty_query_filters.
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
              img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAACFSURBVDhP3ZFLDoAgDETB30oP5Xk8j+fxVu` &&
                            `6kzmAxhIV2QWLiS5p2oKVtcD9iXsXTVJq5C1DcwMm2eFHNu54x4FluHnkHfEXQUWgqzeQrxGKMFs/eHkt59UDHETapNJOv0MEFjBZUP46Yfqse6NjSVJopVzjSaMUKjNPjvB+Qt1/yW5w7AcJ5OMzr` &&
                            `dIT0AAAAAElFTkSuQmCC` ) ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_option_config.
    IF iv_target = zif_sat_c_object_search=>c_search_fields-message_filter_input_key.
      rs_option = mt_message_options[ name = iv_option ].
    ELSE.
      rs_option = super->zif_sat_object_search_config~get_option_config( iv_option ).
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
                                                                                ( get_description_filt_conf( ) )
                                                                                ( get_max_rows_filt_conf( ) ) ).

    mt_message_options = VALUE #( ( get_changed_by_filt_conf( ) )
                                  ( get_changed_on_filt_conf( ) ) ).

    ms_search_type = VALUE #(
        label    = 'Message'
        name     = zif_sat_c_object_search=>c_search_type-message
        img_info = VALUE #( img_key      = zif_sat_c_object_types=>message_class
                            img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        inputs   = VALUE #( ( name           = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label          = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name           = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label          = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters        = lt_object_filters )
                            ( name           = zif_sat_c_object_search=>c_search_fields-message_text_input_key
                              case_sensitive = abap_true
                              label          = zif_sat_c_object_search=>c_search_fields-message_text_input_label )
                            ( name           = zif_sat_c_object_search=>c_search_fields-message_filter_input_key
                              label          = zif_sat_c_object_search=>c_search_fields-message_filter_input_label
                              filters        = mt_message_options ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.
ENDCLASS.
