"! <p class="shorttext synchronized">XML Serializer for Where-Used in CDS entries</p>
CLASS zcl_sat_adt_cdswus_res_xml_h DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Reads external attributes</p>
    CLASS-METHODS read_ext_attributes
      IMPORTING
        io_reader        TYPE REF TO if_sxml_reader
      EXPORTING
        where_used_entry TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds.

    "! <p class="shorttext synchronized">Serializes the given element information</p>
    CLASS-METHODS serialize_ext_attributes
      IMPORTING
        io_writer        TYPE REF TO if_sxml_writer
        where_used_entry TYPE any.
ENDCLASS.


CLASS zcl_sat_adt_cdswus_res_xml_h IMPLEMENTATION.
  METHOD read_ext_attributes.
    DATA ls_child TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds.
    FIELD-SYMBOLS <lt_children> TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds_t.

    io_reader->current_node( ).
    DATA(lv_type) = io_reader->read_next_node( )->type.
    io_reader->push_back( ).

    IF lv_type = if_sxml_node=>co_nt_element_close.
      RETURN.
    ENDIF.

    IF where_used_entry-children IS INITIAL.
      where_used_entry-children = NEW zif_sat_ty_adt_types=>ty_where_used_in_cds_t( ).
    ENDIF.
    ASSIGN where_used_entry-children->* TO <lt_children>.

    TRY.
        WHILE abap_true = abap_true.
          lv_type = io_reader->read_next_node( )->type.
          io_reader->push_back( ).
          IF lv_type = if_sxml_node=>co_nt_element_close.
            EXIT.
          ENDIF.
          CALL TRANSFORMATION zsat_where_used_in_cds_entry
               SOURCE XML io_reader
               RESULT wusl_entry = ls_child.
          APPEND ls_child TO <lt_children>.
        ENDWHILE.
      CATCH cx_sxml_parse_error ##NO_HANDLER.
        " silently ignored
    ENDTRY.
  ENDMETHOD.

  METHOD serialize_ext_attributes.
    FIELD-SYMBOLS <lr_children> TYPE REF TO data.
    FIELD-SYMBOLS <lt_children> TYPE table.

    ASSIGN COMPONENT 'CHILDREN' OF STRUCTURE where_used_entry TO <lr_children>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN <lr_children>->* TO <lt_children>.
    IF sy-subrc = 0.
      LOOP AT <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child>).
        DATA(ls_child) = CORRESPONDING zif_sat_ty_adt_types=>ty_where_used_in_cds( <ls_child> ).
        CALL TRANSFORMATION zsat_where_used_in_cds_entry
             SOURCE wusl_entry = ls_child
             RESULT XML io_writer.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
