"! <p class="shorttext synchronized">Error in ADT DDIC Repository Access</p>
CLASS zcx_sat_adt_ddic_access_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_adt_rest_error FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF invalid_access_mode,
        msgid TYPE symsgid VALUE 'ZSAT_ADT',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_access_mode.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        textid      LIKE if_t100_message=>t100key           OPTIONAL
        !previous   LIKE previous                           OPTIONAL
        subtype     TYPE sadt_exc_type                      OPTIONAL
        msgv1       TYPE symsgv                             DEFAULT sy-msgv1
        msgv2       TYPE symsgv                             DEFAULT sy-msgv2
        msgv3       TYPE symsgv                             DEFAULT sy-msgv3
        msgv4       TYPE symsgv                             DEFAULT sy-msgv4
        !properties TYPE REF TO if_adt_exception_properties OPTIONAL.

    METHODS get_type REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_sat_adt_ddic_access_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous   = previous
                        subtype    = subtype
                        msgv1      = msgv1
                        msgv2      = msgv2
                        msgv3      = msgv3
                        msgv4      = msgv4
                        properties = properties ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD get_type.
    result = 'ddicaccess'.
  ENDMETHOD.
ENDCLASS.
