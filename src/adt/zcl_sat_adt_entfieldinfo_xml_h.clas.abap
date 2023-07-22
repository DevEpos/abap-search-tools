"! <p class="shorttext synchronized">XML Serializer for CDS Top Down child entries</p>
CLASS zcl_sat_adt_entfieldinfo_xml_h DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Reads externala attributes</p>
    CLASS-METHODS read_ext_attributes
      IMPORTING
        io_reader  TYPE REF TO if_sxml_reader
      EXPORTING
        field_info TYPE zif_sat_ty_adt_types=>ty_entity_field_info.

    "! <p class="shorttext synchronized">Serializes the given element information</p>
    CLASS-METHODS serialize_ext_attributes
      IMPORTING
        io_writer  TYPE REF TO if_sxml_writer
        field_info TYPE zif_sat_ty_adt_types=>ty_entity_field_info.
ENDCLASS.


CLASS zcl_sat_adt_entfieldinfo_xml_h IMPLEMENTATION.
  METHOD read_ext_attributes.
    DATA ls_child TYPE zif_sat_ty_adt_types=>ty_entity_field_info.
    FIELD-SYMBOLS <lt_children> TYPE zif_sat_ty_adt_types=>ty_entity_field_infos.

    io_reader->current_node( ).
    DATA(lv_type) = io_reader->read_next_node( )->type.
    io_reader->push_back( ).

    IF lv_type = if_sxml_node=>co_nt_element_close.
      RETURN.
    ENDIF.

    IF field_info-children IS INITIAL.
      field_info-children = NEW zif_sat_ty_adt_types=>ty_entity_field_infos( ).
    ENDIF.
    ASSIGN field_info-children->* TO <lt_children>.

    TRY.
        WHILE abap_true = abap_true.
          lv_type = io_reader->read_next_node( )->type.
          io_reader->push_back( ).
          IF lv_type = if_sxml_node=>co_nt_element_close.
            EXIT.
          ENDIF.
          CALL TRANSFORMATION zsat_adt_entity_field_info
            SOURCE XML io_reader
            RESULT field_info = ls_child.
          APPEND ls_child TO <lt_children>.
        ENDWHILE.
      CATCH cx_sxml_parse_error ##NO_HANDLER.
        " silently ignored
    ENDTRY.
  ENDMETHOD.

  METHOD serialize_ext_attributes.
    FIELD-SYMBOLS <lt_children> TYPE zif_sat_ty_adt_types=>ty_entity_field_infos.

    CHECK field_info-children IS BOUND.

    ASSIGN field_info-children->* TO <lt_children>.

    IF <lt_children> IS ASSIGNED.

      LOOP AT <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child>).
        CALL TRANSFORMATION zsat_adt_entity_field_info
         SOURCE field_info = <ls_child>
         RESULT XML io_writer.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
