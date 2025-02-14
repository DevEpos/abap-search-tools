CLASS zcx_sat_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_nc_exception FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error.

    METHODS constructor
      IMPORTING
        textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous                 OPTIONAL
        msgv1     TYPE sy-msgv1                 OPTIONAL
        msgv2     TYPE sy-msgv2                 OPTIONAL
        msgv3     TYPE sy-msgv3                 OPTIONAL
        msgv4     TYPE sy-msgv4                 OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_sat_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous
                        msgv1    = msgv1
                        msgv2    = msgv2
                        msgv3    = msgv3
                        msgv4    = msgv4 ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
