CLASS zcx_sat_validation_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_nc_exception
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF package_not_existing,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF package_not_existing.
    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error.
    CONSTANTS:
      BEGIN OF no_mapping_defined,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_mapping_defined.
    CONSTANTS:
      BEGIN OF no_field_specified,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_field_specified.
    CONSTANTS:
      BEGIN OF field_exists_not_in_table,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_exists_not_in_table.
    CONSTANTS:
      BEGIN OF field_does_not_exists,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_does_not_exists.
    CONSTANTS:
      BEGIN OF mandatory_fields_empty,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mandatory_fields_empty.
    CONSTANTS:
      BEGIN OF duplicate_entries_exist,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_entries_exist.
    CONSTANTS:
      BEGIN OF field_mismatch,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_mismatch.
    CONSTANTS:
      BEGIN OF parameter_value_missing,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parameter_value_missing.

    DATA parameter_name TYPE dynfnam READ-ONLY.
    DATA loop_line TYPE sy-tabix READ-ONLY.

    METHODS constructor
      IMPORTING
        textid         LIKE if_t100_message=>t100key OPTIONAL
        !previous      LIKE previous                 OPTIONAL
        msgv1          TYPE sy-msgv1                 OPTIONAL
        msgv2          TYPE sy-msgv2                 OPTIONAL
        msgv3          TYPE sy-msgv3                 OPTIONAL
        msgv4          TYPE sy-msgv4                 OPTIONAL
        parameter_name TYPE dynfnam                  OPTIONAL
        loop_line      TYPE sy-tabix                 OPTIONAL.

    CLASS-METHODS raise_from_sy
      IMPORTING
        iv_parameter TYPE dynfnam OPTIONAL
        iv_line      TYPE i       OPTIONAL.

    CLASS-METHODS raise_with_text
      IMPORTING
        iv_text      TYPE string
        iv_parameter TYPE dynfnam OPTIONAL
        iv_line      TYPE i       OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_sat_validation_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous
                        msgv1    = msgv1
                        msgv2    = msgv2
                        msgv3    = msgv3
                        msgv4    = msgv4 ).
    me->parameter_name = parameter_name.
    me->loop_line      = loop_line.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_from_sy.
    RAISE EXCEPTION TYPE zcx_sat_validation_exception
      EXPORTING textid         = VALUE scx_t100key( msgid = sy-msgid
                                                    msgno = sy-msgno
                                                    attr1 = 'MSGV1'
                                                    attr2 = 'MSGV2'
                                                    attr3 = 'MSGV3'
                                                    attr4 = 'MSGV4' )
                msgv1          = sy-msgv1
                msgv2          = sy-msgv2
                msgv3          = sy-msgv3
                msgv4          = sy-msgv4
                parameter_name = iv_parameter
                loop_line      = iv_line.
  ENDMETHOD.

  METHOD raise_with_text.
    zcl_sat_message_helper=>split_string_for_message( EXPORTING iv_string = iv_text
                                                      IMPORTING ev_msgv1  = DATA(lv_msgv1)
                                                                ev_msgv2  = DATA(lv_msgv2)
                                                                ev_msgv3  = DATA(lv_msgv3)
                                                                ev_msgv4  = DATA(lv_msgv4) ).

    RAISE EXCEPTION TYPE zcx_sat_validation_exception
      EXPORTING textid         = general_error
                msgv1          = lv_msgv1
                msgv2          = lv_msgv2
                msgv3          = lv_msgv3
                msgv4          = lv_msgv4
                parameter_name = iv_parameter
                loop_line      = iv_line.
  ENDMETHOD.
ENDCLASS.
