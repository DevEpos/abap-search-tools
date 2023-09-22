"! <p class="shorttext synchronized" lang="en">Reads element information for a CDS view</p>
CLASS zcl_sat_adt_cds_elinfo_reader DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_adt_elinfo_reader.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_request       TYPE REF TO if_adt_rest_request
        iv_cds_view_name TYPE zsat_cds_view_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_association.
        INCLUDE TYPE zsat_cds_association.
    TYPES:
      ddlname     TYPE ddlname,
      source_type TYPE char1.
    TYPES: END OF ty_s_association.
    TYPES: ty_t_association TYPE STANDARD TABLE OF ty_s_association WITH EMPTY KEY.

    DATA mo_request TYPE REF TO if_adt_rest_request.
    DATA mv_cds_view_name TYPE zsat_cds_view_name.
    DATA ms_element_info TYPE zsat_adt_cds_info.
    DATA mf_show_association_name TYPE abap_bool VALUE abap_true.
    DATA mv_ddl_view TYPE zsat_cds_view_header-ddlname.

    "! <p class="shorttext synchronized" lang="en">Retrieve association of cds view (with ddlnames)</p>
    METHODS get_cds_associations
      IMPORTING
        io_cds_view           TYPE REF TO zcl_sat_cds_view
      RETURNING
        VALUE(rt_association) TYPE ty_t_association.


    "! <p class="shorttext synchronized" lang="en">Fills Base table element information</p>
    METHODS fill_base_table_infos
      IMPORTING
        it_base_tables TYPE zsat_cds_view_base_table_t .
    "! <p class="shorttext synchronized" lang="en">Fills association element information </p>
    METHODS fill_association_infos
      IMPORTING
        it_association TYPE ty_t_association.
    "! <p class="shorttext synchronized" lang="en">Read additional request parameters</p>
    METHODS get_parameters
      RAISING
        cx_adt_rest.
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if this view is an analytics query</p>
    METHODS is_analytics_query
      IMPORTING
        io_cds_view        TYPE REF TO zcl_sat_cds_view
      RETURNING
        VALUE(rv_is_query) TYPE string.
ENDCLASS.



CLASS zcl_sat_adt_cds_elinfo_reader IMPLEMENTATION.

  METHOD constructor.
    mo_request = io_request.
    mv_cds_view_name = iv_cds_view_name.
  ENDMETHOD.

  METHOD zif_sat_adt_elinfo_reader~read_element_information.
    TRY.
        get_parameters( ).

        DATA(lo_cds_view) = zcl_sat_cds_view_factory=>read_cds_view( iv_cds_view = mv_cds_view_name ).
        DATA(lt_associations) = get_cds_associations( lo_cds_view ).
        DATA(lt_base_entities) = lo_cds_view->get_base_tables( ).
        DATA(ls_header) = lo_cds_view->get_header( ).
        mv_ddl_view = ls_header-ddlname.
        DATA(ls_tadir_props) = lo_cds_view->get_tadir_info( ).

        ms_element_info = VALUE #(
           name         = lo_cds_view->mv_view_name
           raw_name     = ls_header-entityname_raw
           is_query     = is_analytics_query( lo_cds_view )
           properties   = VALUE #(
              package      = ls_tadir_props-devclass
              owner        = ls_tadir_props-created_by
              created_date = ls_tadir_props-created_date
              changed_date = ls_header-chgdate
           )
        ).

        fill_base_table_infos( lt_base_entities ).
        fill_association_infos( lt_associations ).

        io_rest_response->set_body_data(
            content_handler = zcl_sat_adt_ch_factory=>create_cds_elem_info_res_ch( )
            data            = ms_element_info
        ).
      CATCH zcx_sat_data_read_error INTO DATA(lx_read_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_read_error ).
      CATCH cx_adt_rest INTO DATA(lx_rest).
        RAISE EXCEPTION TYPE zcx_sat_adt_element_info
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_rest ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_parameters.
    mf_show_association_name = zcl_sat_adt_res_util=>get_boolean_req_param(
       iv_param_name = zif_sat_c_adt_utils=>c_cds_elem_info_parameter-show_association_name
       io_request    = mo_request
    ).
  ENDMETHOD.

  METHOD get_cds_associations.
    DATA: lt_entity_range TYPE RANGE OF zsat_entity_id.

    CHECK io_cds_view->has_associations( ).

    rt_association = CORRESPONDING #( io_cds_view->get_associations( ) ).
    lt_entity_range = VALUE #( FOR ref_cds IN rt_association WHERE ( ref_cds_view <> space )
                               ( sign = 'I' option = 'EQ' low = ref_cds-ref_cds_view ) ).

    IF mf_show_association_name = abap_false.
      SORT rt_association BY ref_cds_view.
      DELETE ADJACENT DUPLICATES FROM rt_association COMPARING ref_cds_view.
      LOOP AT rt_association ASSIGNING FIELD-SYMBOL(<ls_assoc>).
        CLEAR: <ls_assoc>-name,
               <ls_assoc>-raw_name.
      ENDLOOP.
    ENDIF.

    IF lt_entity_range IS NOT INITIAL.
      SELECT ddlname,
             entityid,
             sourcetype
        FROM zsat_i_cdsentity( p_language = @sy-langu )
        WHERE entityid IN @lt_entity_range
      INTO TABLE @DATA(lt_ddlname_map).
    ENDIF.

    LOOP AT lt_ddlname_map ASSIGNING FIELD-SYMBOL(<ls_ddl_map>).
      LOOP AT rt_association ASSIGNING <ls_assoc> WHERE ref_cds_view = <ls_ddl_map>-entityid.
        <ls_assoc>-ddlname = <ls_ddl_map>-ddlname.
        <ls_assoc>-source_type = <ls_ddl_map>-sourcetype.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_association_infos.
    LOOP AT it_association ASSIGNING FIELD-SYMBOL(<ls_assoc>).
      APPEND INITIAL LINE TO ms_element_info-associations ASSIGNING FIELD-SYMBOL(<ls_assoc_line>).
      <ls_assoc_line>-name = <ls_assoc>-name.
      <ls_assoc_line>-raw_name = <ls_assoc>-raw_name.
      <ls_assoc_line>-object = VALUE #(
         name         = <ls_assoc>-ref_cds_view
         raw_name     = <ls_assoc>-ref_cds_view_raw
         source_type  = <ls_assoc>-source_type
         object_type  = <ls_assoc>-entity_type
         description  = <ls_assoc>-ddtext
      ).
      DATA(ls_object_ref) = zcl_sat_adt_util=>create_adt_uri(
         iv_type  = <ls_assoc>-entity_type
         iv_name  = <ls_assoc>-ref_cds_view
         iv_name2 = CONV #( <ls_assoc>-ddlname )
      ).
      <ls_assoc_line>-object-uri = ls_object_ref-uri.
      <ls_assoc_line>-object-adt_type = ls_object_ref-type.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_base_table_infos.
    LOOP AT it_base_tables ASSIGNING FIELD-SYMBOL(<ls_base_entity>).
      APPEND INITIAL LINE TO ms_element_info-select_from ASSIGNING FIELD-SYMBOL(<ls_select_from>).
      <ls_select_from> = VALUE #(
         name         = <ls_base_entity>-entityname
         raw_name     = <ls_base_entity>-entityname_raw
         object_type  = <ls_base_entity>-table_kind
         source_type  = <ls_base_entity>-source_type
         description  = <ls_base_entity>-description
      ).
      DATA(ls_object_ref) = zcl_sat_adt_util=>create_adt_uri(
         iv_type  = <ls_base_entity>-table_kind
         iv_name  = <ls_base_entity>-entityname
         iv_name2 = <ls_base_entity>-secondary_entity_id
      ).
      <ls_select_from>-uri = ls_object_ref-uri.
      <ls_select_from>-adt_type = ls_object_ref-type.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_analytics_query.
    rv_is_query = 'false'.
    CHECK io_cds_view IS BOUND.

    IF io_cds_view->is_analytics_query( ).
      rv_is_query = 'true'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
