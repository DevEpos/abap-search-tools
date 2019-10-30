"! <p class="shorttext synchronized" lang="en">Reads element information for a Database View</p>
CLASS zcl_sat_adt_view_elinfo_rdr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_adt_elinfo_reader.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_request  TYPE REF TO if_adt_rest_request
        iv_viewname TYPE viewname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_request TYPE REF TO if_adt_rest_request.
    DATA mv_viewname TYPE viewname.
    DATA mv_language TYPE sy-langu.
ENDCLASS.



CLASS zcl_sat_adt_view_elinfo_rdr IMPLEMENTATION.

  METHOD constructor.
    mo_request = io_request.
    mv_viewname = iv_viewname.
  ENDMETHOD.

  METHOD zif_sat_adt_elinfo_reader~read_element_information.
    SELECT SINGLE *
      FROM zsat_i_databaseview
      WHERE viewname = @mv_viewname
    INTO @DATA(ls_entity).

    CHECK sy-subrc = 0.

    TRY.
        io_rest_response->set_body_data(
          content_handler = zcl_sat_adt_ch_factory=>create_view_elem_info_res_ch( )
          data            = VALUE zsat_adt_db_view_info(
              name         = ls_entity-viewname
              properties   = VALUE #(
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
