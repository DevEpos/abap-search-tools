"! <p class="shorttext synchronized">Seriliazer for Element Information children property</p>
CLASS zcl_sat_adt_elem_info_serlzer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Reads externala attributes</p>
    CLASS-METHODS read_ext_attributes
      IMPORTING
        io_reader           TYPE REF TO if_sxml_reader
        transformation_name TYPE string
      EXPORTING
        code_element        TYPE zsat_adt_element_info.

    "! <p class="shorttext synchronized">Serializes the given element information</p>
    CLASS-METHODS serialize_ext_attributes
      IMPORTING
        io_writer           TYPE REF TO if_sxml_writer
        code_element        TYPE zsat_adt_element_info
        transformation_name TYPE string.
ENDCLASS.


CLASS zcl_sat_adt_elem_info_serlzer IMPLEMENTATION.
  METHOD read_ext_attributes.
    DATA ls_child TYPE zsat_adt_element_info.
    FIELD-SYMBOLS <lt_children> TYPE zsat_adt_element_info_t.

    io_reader->current_node( ).
    DATA(lv_type) = io_reader->read_next_node( )->type.
    io_reader->push_back( ).

    IF lv_type = if_sxml_node=>co_nt_element_close.
      RETURN.
    ENDIF.

    IF code_element-children IS INITIAL.
      code_element-children = NEW zsat_adt_element_info_t( ).
    ENDIF.
    ASSIGN code_element-children->* TO <lt_children>.

    TRY.
        WHILE abap_true = abap_true.
          lv_type = io_reader->read_next_node( )->type.
          io_reader->push_back( ).
          IF lv_type = if_sxml_node=>co_nt_element_close.
            EXIT.
          ENDIF.
          CALL TRANSFORMATION (transformation_name)
               SOURCE XML io_reader
               RESULT element_info = ls_child.
          APPEND ls_child TO <lt_children>.
        ENDWHILE.
      CATCH cx_sxml_parse_error ##NO_HANDLER.
        " silently ignored
    ENDTRY.
  ENDMETHOD.

  METHOD serialize_ext_attributes.
    FIELD-SYMBOLS <lt_children> TYPE zsat_adt_element_info_t.

    CHECK code_element-children IS BOUND.

    ASSIGN code_element-children->* TO <lt_children>.

    IF <lt_children> IS ASSIGNED.

      LOOP AT <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child>).
        CALL TRANSFORMATION (transformation_name)
             SOURCE element_info = <ls_child>
             RESULT XML io_writer.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
