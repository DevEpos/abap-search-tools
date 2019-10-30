"! <p class="shorttext synchronized" lang="en">Generic ADT REST Error in Search and Analyis tools</p>
CLASS zcx_sat_adt_rest_error DEFINITION
  PUBLIC
  INHERITING FROM cx_adt_rest
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF object_type_not_supported,
        msgid TYPE symsgid VALUE 'ZSAT_ADT',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF object_type_not_supported .
    CONSTANTS:
      BEGIN OF missing_parameter,
        msgid TYPE symsgid VALUE 'ZSAT_ADT',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_parameter .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !subtype    TYPE sadt_exc_type OPTIONAL
        !msgv1      TYPE symsgv DEFAULT sy-msgv1
        !msgv2      TYPE symsgv DEFAULT sy-msgv2
        !msgv3      TYPE symsgv DEFAULT sy-msgv3
        !msgv4      TYPE symsgv DEFAULT sy-msgv4
        !properties TYPE REF TO if_adt_exception_properties OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_sat_adt_rest_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous   = previous
        subtype    = subtype
        msgv1      = msgv1
        msgv2      = msgv2
        msgv3      = msgv3
        msgv4      = msgv4
        properties = properties.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
