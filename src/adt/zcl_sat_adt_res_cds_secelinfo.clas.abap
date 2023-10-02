"! <p class="shorttext synchronized">Resource for reading secondary information of CDS view</p>
CLASS zcl_sat_adt_res_cds_secelinfo DEFINITION
  PUBLIC
    INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_cds_view_name TYPE zsat_cds_view_name.
    DATA mv_ddl_view TYPE ddlname.
    DATA ms_secondary_element_info TYPE zsat_adt_cds_second_info.

    "! <p class="shorttext synchronized">Load the extension files</p>
    METHODS load_extensions.
    "! <p class="shorttext synchronized">Load the access controls of the CDS view</p>
    METHODS load_access_controls.

    "! <p class="shorttext synchronized">Get request parameters</p>
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        zcx_sat_adt_element_info.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_secelinfo IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).

    SELECT
      SINGLE ddlname
      FROM zsat_p_cdsviewbase
      WHERE entityid = @mv_cds_view_name
    INTO @mv_ddl_view.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    load_extensions( ).
    load_access_controls( ).

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_cds_secondary_res_ch( )
                             data            = ms_secondary_element_info ).
  ENDMETHOD.

  METHOD get_parameters.
    TRY.
        io_request->get_uri_query_parameter( EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-name
                                                       mandatory = abap_true
                                             IMPORTING value     = mv_cds_view_name ).
      CATCH cx_adt_rest INTO DATA(lx_rest_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_rest_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD load_extensions.
    SELECT base~ddlname,
           base~rawentityid,
           base~entityid,
           text~ddtext
      FROM zsatpcdsvb AS base
        INNER JOIN ddddlsrc AS ddlsource
          ON base~ddlname = ddlsource~ddlname
        LEFT OUTER JOIN ddddlsrct AS text
          ON  ddlsource~ddlname    = text~ddlname
          AND text~ddlanguage      = @sy-langu
      WHERE ddlsource~parentname = @mv_ddl_view
      ORDER BY base~ddlname
    INTO TABLE @DATA(lt_extends).

    LOOP AT lt_extends ASSIGNING FIELD-SYMBOL(<ls_extend>).
      APPEND INITIAL LINE TO ms_secondary_element_info-extensions ASSIGNING FIELD-SYMBOL(<ls_adt_object>).

      <ls_adt_object> = VALUE #( name        = <ls_extend>-entityid
                                 raw_name    = <ls_extend>-rawentityid
                                 description = <ls_extend>-ddtext
                                 adt_type    = 'DDLS/DF'
                                 uri         = zcl_sat_adt_util=>get_adt_object_ref_uri(
                                                   iv_name = CONV #( <ls_extend>-ddlname )
                                                   is_type = VALUE #( objtype_tr = 'DDLS' subtype_wb = 'DF' ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD load_access_controls.
    SELECT role~dclname,
           text~ddtext
      FROM acmrole AS role
        LEFT OUTER JOIN acmdclsrct AS text
          ON  role~dclname = text~dclname
          AND text~dcllanguage = @sy-langu
      WHERE role~name = @mv_cds_view_name
      ORDER BY role~dclname
    INTO TABLE @DATA(lt_access_control).

    LOOP AT lt_access_control ASSIGNING FIELD-SYMBOL(<ls_access_control>).
      APPEND INITIAL LINE TO ms_secondary_element_info-access_controls ASSIGNING FIELD-SYMBOL(<ls_adt_object>).
      <ls_adt_object> = VALUE #( name        = <ls_access_control>-dclname
                                 raw_name    = <ls_access_control>-dclname
                                 description = <ls_access_control>-ddtext
                                 adt_type    = 'DCLS/DL'
                                 uri         = zcl_sat_adt_util=>get_adt_object_ref_uri(
                                                   iv_name = CONV #( <ls_access_control>-dclname )
                                                   is_type = VALUE #( objtype_tr = 'DCLS' subtype_wb = 'DL' ) ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
