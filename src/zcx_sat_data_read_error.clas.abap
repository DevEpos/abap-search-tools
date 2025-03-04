CLASS zcx_sat_data_read_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_sat_application_exc
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cds_view_not_existing,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cds_view_not_existing.

    CONSTANTS:
      BEGIN OF db_table_view_not_existing,
        msgid TYPE symsgid VALUE 'ZSAT_EXCEPTION',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF db_table_view_not_existing.

    CLASS-METHODS raise_data_read_error_sy
      RAISING
        zcx_sat_data_read_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_sat_data_read_error IMPLEMENTATION.
  METHOD raise_data_read_error_sy.
    RAISE EXCEPTION TYPE zcx_sat_data_read_error
      EXPORTING textid = VALUE scx_t100key( msgid = sy-msgid
                                            msgno = sy-msgno
                                            attr1 = 'MSGV1'
                                            attr2 = 'MSGV2'
                                            attr3 = 'MSGV3'
                                            attr4 = 'MSGV4' )
                msgv1  = sy-msgv1
                msgv2  = sy-msgv2
                msgv3  = sy-msgv3
                msgv4  = sy-msgv4.
  ENDMETHOD.
ENDCLASS.
