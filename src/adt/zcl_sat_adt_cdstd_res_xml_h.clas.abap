"! <p class="shorttext synchronized">XML Serializer for CDS Top Down child entries</p>
CLASS zcl_sat_adt_cdstd_res_xml_h DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Reads externala attributes</p>
    CLASS-METHODS read_ext_attributes
      IMPORTING
        io_reader      TYPE REF TO if_sxml_reader
      EXPORTING
        top_down_entry TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entry.

    "! <p class="shorttext synchronized">Serializes the given element information</p>
    CLASS-METHODS serialize_ext_attributes
      IMPORTING
        io_writer      TYPE REF TO if_sxml_writer
        top_down_entry TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entry.
ENDCLASS.


CLASS zcl_sat_adt_cdstd_res_xml_h IMPLEMENTATION.
  METHOD read_ext_attributes.
    DATA ls_child TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entry.
    FIELD-SYMBOLS <lt_children> TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entries.

    io_reader->current_node( ).
    DATA(lv_type) = io_reader->read_next_node( )->type.
    io_reader->push_back( ).

    IF lv_type = if_sxml_node=>co_nt_element_close.
      RETURN.
    ENDIF.

    IF top_down_entry-children IS INITIAL.
      top_down_entry-children = NEW zif_sat_ty_adt_types=>ty_cds_top_down_entries( ).
    ENDIF.
    ASSIGN top_down_entry-children->* TO <lt_children>.

    TRY.
        WHILE abap_true = abap_true.
          lv_type = io_reader->read_next_node( )->type.
          io_reader->push_back( ).
          IF lv_type = if_sxml_node=>co_nt_element_close.
            EXIT.
          ENDIF.
          CALL TRANSFORMATION zsat_topdown_analysis_entry
            SOURCE XML io_reader
            RESULT top_down_entry = ls_child.
          APPEND ls_child TO <lt_children>.
        ENDWHILE.
      CATCH cx_sxml_parse_error ##NO_HANDLER.
        " silently ignored
    ENDTRY.
  ENDMETHOD.

  METHOD serialize_ext_attributes.
    FIELD-SYMBOLS <lt_children> TYPE zif_sat_ty_adt_types=>ty_cds_top_down_entries.

    CHECK top_down_entry-children IS BOUND.

    ASSIGN top_down_entry-children->* TO <lt_children>.

    IF <lt_children> IS ASSIGNED.

      LOOP AT <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child>).
        CALL TRANSFORMATION zsat_topdown_analysis_entry
         SOURCE top_down_entry = <ls_child>
         RESULT XML io_writer.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
