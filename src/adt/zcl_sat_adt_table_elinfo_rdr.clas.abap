"! <p class="shorttext synchronized" lang="en">Reads element information for a Database Table</p>
CLASS zcl_sat_adt_table_elinfo_rdr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_adt_elinfo_reader.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
        iv_tabname TYPE tabname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_tabname TYPE tabname.
    DATA mo_request TYPE REF TO if_adt_rest_request.

ENDCLASS.



CLASS zcl_sat_adt_table_elinfo_rdr IMPLEMENTATION.

  METHOD constructor.
    mo_request = io_request.
    mv_tabname = iv_tabname.
  ENDMETHOD.

  METHOD zif_sat_adt_elinfo_reader~read_element_information.
    SELECT SINGLE *
      FROM zsat_i_databaseentity
      WHERE entity = @mv_tabname
    INTO @DATA(ls_entity).

    CHECK sy-subrc = 0.

    DATA(ls_tech_settings) = VALUE zsat_adt_object_info(
        name     = mv_tabname
        raw_name = mv_tabname
        uri      = zcl_sat_adt_util=>get_adt_object_ref_uri(
          iv_name = CONV #( mv_tabname )
          is_type = VALUE #( objtype_tr = 'TABL' subtype_wb = 'DTT' ) )
        adt_type  = 'TABL/DTT'
    ).
    IF ls_tech_settings-uri IS INITIAL.
      CLEAR ls_tech_settings.
    ENDIF.

    TRY.
        io_rest_response->set_body_data(
          content_handler = zcl_sat_adt_ch_factory=>create_table_elem_info_res_ch( )
          data            = VALUE zsat_adt_db_table_info(
              name                   = ls_entity-entity
*............ Create URI for the technical settings of this table
              technical_settings = ls_tech_settings
              properties             = VALUE #(
                package      = ls_entity-developmentpackage
                owner        = ls_entity-createdby
                created_date = ls_entity-createddate
                changed_date = ls_entity-changeddate
              )
          )
      ).
      CATCH cx_adt_rest INTO DATA(lx_rest).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_rest ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
