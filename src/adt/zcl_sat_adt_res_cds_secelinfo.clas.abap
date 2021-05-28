"! <p class="shorttext synchronized" lang="en">Resource for reading secondary information of CDS view</p>
CLASS zcl_sat_adt_res_cds_secelinfo DEFINITION
  PUBLIC
    INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_cds_view_name TYPE zsat_cds_view_name.
    DATA mv_ddl_view TYPE ddlname.
    DATA ms_secondary_element_info TYPE zsat_adt_cds_second_info.

    "! <p class="shorttext synchronized" lang="en">Load the Metadata extension files</p>
    METHODS load_metadata_ext.
    "! <p class="shorttext synchronized" lang="en">Load the extension files</p>
    METHODS load_extensions.
    "! <p class="shorttext synchronized" lang="en">Load the access controls of the CDS view</p>
    METHODS load_access_controls.
    "! <p class="shorttext synchronized" lang="en">Load the business object(s) of the CDS view</p>
    METHODS load_business_object.
    "! <p class="shorttext synchronized" lang="en">Load the referenced/used classes of the CDS view</p>
    METHODS load_referenced_classes.

    "! <p class="shorttext synchronized" lang="en">Get request parameters</p>
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
      SINGLE
      FROM zsat_p_cdsviewbase
      FIELDS ddlname
      WHERE entityid = @mv_cds_view_name
    INTO @mv_ddl_view.

    CHECK sy-subrc = 0.

    load_extensions( ).
    load_metadata_ext( ).
    load_access_controls( ).
    load_referenced_classes( ).
    load_business_object( ).

    response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_cds_secondary_res_ch( )
        data            = ms_secondary_element_info
    ).
  ENDMETHOD.

  METHOD get_parameters.

    TRY.
        io_request->get_uri_query_parameter(
          EXPORTING name      = zif_sat_c_adt_utils=>c_element_info_parameter-name
                    mandatory = abap_true
          IMPORTING value     = mv_cds_view_name
        ).
      CATCH cx_adt_rest INTO DATA(lx_rest_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_rest_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD load_metadata_ext.
    SELECT
      FROM ddlx_rt_header AS metadata_header
        LEFT OUTER JOIN ddlxsrct AS text
          ON  metadata_header~ddlxname = text~ddlxname
          AND text~language = @sy-langu
      FIELDS metadata_header~ddlxname,
             text~description
      WHERE metadata_header~extended_artifact = @mv_cds_view_name
    INTO TABLE @DATA(lt_metadata_ext).

    LOOP AT lt_metadata_ext ASSIGNING FIELD-SYMBOL(<ls_metadata_ext>).
      APPEND INITIAL LINE TO ms_secondary_element_info-metadata_ext ASSIGNING FIELD-SYMBOL(<ls_adt_object>).
      <ls_adt_object> = VALUE #(
        name        = <ls_metadata_ext>-ddlxname
        raw_name    = <ls_metadata_ext>-ddlxname
        description = <ls_metadata_ext>-description
        adt_type    = 'DDLX/EX'
        uri         = zcl_sat_adt_util=>get_adt_object_ref_uri(
          iv_name = CONV #( <ls_metadata_ext>-ddlxname )
          is_type = VALUE #( objtype_tr = 'DDLX' subtype_wb = 'EX' )
        )
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD load_extensions.
    SELECT
      FROM zsat_p_cdsviewbase AS base
        INNER JOIN ddddlsrc AS ddlsource
          ON base~ddlname = ddlsource~ddlname
        LEFT OUTER JOIN ddddlsrc02bt AS text
          ON  ddlsource~ddlname    = text~ddlname
          AND text~ddlanguage      = @sy-langu
      FIELDS base~ddlname,
             base~rawentityid,
             base~entityid,
             text~ddtext
      WHERE ddlsource~parentname = @mv_ddl_view
      ORDER BY base~ddlname
    INTO TABLE @DATA(lt_extends).

    LOOP AT lt_extends ASSIGNING FIELD-SYMBOL(<ls_extend>).
      APPEND INITIAL LINE TO ms_secondary_element_info-extensions ASSIGNING FIELD-SYMBOL(<ls_adt_object>).

      <ls_adt_object> = VALUE #(
        name        = <ls_extend>-entityid
        raw_name    = <ls_extend>-rawentityid
        description = <ls_extend>-ddtext
        adt_type    = 'DDLS/DF'
        uri         = zcl_sat_adt_util=>get_adt_object_ref_uri(
          iv_name = CONV #( <ls_extend>-ddlname )
          is_type = VALUE #( objtype_tr = 'DDLS' subtype_wb = 'DF' )
        )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD load_access_controls.
    SELECT
      FROM acmrole AS role
        LEFT OUTER JOIN acmdclsrct AS text
          ON  role~dclname = text~dclname
          AND text~dcllanguage = @sy-langu
      FIELDS role~dclname,
             text~ddtext
      WHERE role~name = @mv_cds_view_name
    INTO TABLE @DATA(lt_access_control).

    SELECT
      FROM acmdclmetadataie AS inherited_role
        LEFT OUTER JOIN acmdclsrct AS text
          ON  inherited_role~artifactname = text~dclname
          AND text~dcllanguage = @sy-langu
      FIELDS inherited_role~artifactname,
             text~ddtext
      WHERE inherited_role~inherit_from_entityname = @mv_cds_view_name
    APPENDING TABLE @lt_access_control.

    SORT lt_access_control BY dclname.

    LOOP AT lt_access_control ASSIGNING FIELD-SYMBOL(<ls_access_control>).
      APPEND INITIAL LINE TO ms_secondary_element_info-access_controls ASSIGNING FIELD-SYMBOL(<ls_adt_object>).
      <ls_adt_object> = VALUE #(
        name        = <ls_access_control>-dclname
        raw_name    = <ls_access_control>-dclname
        description = <ls_access_control>-ddtext
        adt_type    = 'DCLS/DL'
        uri         = zcl_sat_adt_util=>get_adt_object_ref_uri(
          iv_name = CONV #( <ls_access_control>-dclname )
          is_type = VALUE #( objtype_tr = 'DCLS' subtype_wb = 'DL' )
        )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD load_business_object.
    SELECT
      SINGLE
      FROM /bobf/obm_node
      FIELDS name
      WHERE object_model_cds_view_name = @mv_cds_view_name
    INTO @DATA(lv_bo).

    CHECK sy-subrc = 0.

    ms_secondary_element_info-business_object = VALUE #(
      name        = lv_bo
      raw_name    = lv_bo
      adt_type = 'BOBF'
      uri         = zcl_sat_adt_util=>get_adt_object_ref_uri(
        iv_name = CONV #( lv_bo )
        is_type = VALUE #( objtype_tr = 'BOBF' )
      )
    ).
  ENDMETHOD.


  METHOD load_referenced_classes.
    SELECT
      FROM zsat_i_cdsreferencedclsinanno AS refs
        LEFT OUTER JOIN seoclasstx AS cls
          ON  refs~value = cls~clsname
          AND cls~langu = @sy-langu
      FIELDS DISTINCT
             cls~clsname,
             cls~descript
      WHERE refs~entityid = @mv_cds_view_name
      ORDER BY cls~clsname
    INTO TABLE @DATA(lt_classes).

    LOOP AT lt_classes ASSIGNING FIELD-SYMBOL(<ls_class>) WHERE clsname IS NOT INITIAL.
      APPEND INITIAL LINE TO ms_secondary_element_info-referenced_classes ASSIGNING FIELD-SYMBOL(<ls_adt_object>).

      DATA(ls_adt_object) = zcl_sat_adt_util=>create_adt_uri(
        iv_tadir_type = 'CLAS'
        iv_name       = <ls_class>-clsname
      ).

      <ls_adt_object> = VALUE #(
        name        = <ls_class>-clsname
        raw_name    = <ls_class>-clsname
        description = <ls_class>-descript
        adt_type    = ls_adt_object-type
        uri         = ls_adt_object-uri
      ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
