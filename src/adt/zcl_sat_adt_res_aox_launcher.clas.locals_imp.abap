*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_dummy_content_handler IMPLEMENTATION.
  METHOD if_adt_rest_content_handler~deserialize.
    RETURN.
  ENDMETHOD.

  METHOD if_adt_rest_content_handler~get_supported_content_type.
    result = if_rest_media_type=>gc_text_plain.
  ENDMETHOD.

  METHOD if_adt_rest_content_handler~serialize.
    DATA(lt_params) = VALUE tihttpnvp( ( name  = 'charset'
                                         value = 'utf-8'  ) ).

    response_entity->set_binary_data( mv_launcher_xml ).
    response_entity->set_content_type( iv_media_type = me->if_adt_rest_content_handler~get_supported_content_type( )
                                       it_parameter  = lt_params ).
  ENDMETHOD.

  METHOD constructor.
    mv_launcher_xml = iv_launcher_xml.
  ENDMETHOD.
ENDCLASS.
