"! <p class="shorttext synchronized" lang="en">General error during element information retrieval</p>
CLASS zcx_sat_adt_element_info DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_adt_rest_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        textid     LIKE if_t100_message=>t100key OPTIONAL
        previous   LIKE previous OPTIONAL
        subtype    TYPE sadt_exc_type OPTIONAL
        msgv1      TYPE symsgv DEFAULT sy-msgv1
        msgv2      TYPE symsgv DEFAULT sy-msgv2
        msgv3      TYPE symsgv DEFAULT sy-msgv3
        msgv4      TYPE symsgv DEFAULT sy-msgv4
        properties TYPE REF TO if_adt_exception_properties OPTIONAL .
    METHODS get_type
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_sat_adt_element_info IMPLEMENTATION.


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

  METHOD get_type.
    result = 'elementinfo'.
  ENDMETHOD.

ENDCLASS.
