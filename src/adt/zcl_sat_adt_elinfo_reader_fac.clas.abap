"! <p class="shorttext synchronized" lang="en">Factory for creating element information readers</p>
CLASS zcl_sat_adt_elinfo_reader_fac DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates element information reader</p>
    CLASS-METHODS create_reader
      IMPORTING
        io_request       TYPE REF TO if_adt_rest_request
        iv_type          TYPE zsat_entity_type
        iv_name          TYPE zsat_entity_id
      RETURNING
        VALUE(ro_reader) TYPE REF TO zif_sat_adt_elinfo_reader.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_elinfo_reader_fac IMPLEMENTATION.
  METHOD create_reader.
    CASE iv_type.

      WHEN zif_sat_c_entity_type=>cds_view.
        ro_reader = NEW zcl_sat_adt_cds_elinfo_reader(
          io_request       = io_request
          iv_cds_view_name = iv_name
        ).

      WHEN zif_sat_c_entity_type=>table.
        ro_reader = NEW zcl_sat_adt_table_elinfo_rdr(
          io_request  = io_request
          iv_tabname  = iv_name
        ).

      WHEN zif_sat_c_entity_type=>view.
        ro_reader = NEW zcl_sat_adt_view_elinfo_rdr(
          io_request  = io_request
          iv_viewname = iv_name
        ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
