"! <p class="shorttext synchronized" lang="en">Resource for reading Field Hierarchy of Database Field</p>
CLASS zcl_sat_adt_res_dbfield_hier DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_field_mode,
        hierarchy  TYPE string VALUE 'hierarchy',
        where_used TYPE string VALUE 'whereUsed',
      END OF c_field_mode.
    CONSTANTS:
      c_field_name    TYPE string VALUE 'FIELD' ##NO_TEXT,
      c_source_type   TYPE string VALUE 'SOURCE_TYPE' ##NO_TEXT,
      c_api_state     TYPE string VALUE 'API_STATE' ##NO_TEXT,
      c_is_calculated TYPE string VALUE 'IS_CALCULATED' ##NO_TEXT.

    DATA mv_field TYPE fieldname.
    DATA mv_object_name TYPE zsat_entity_id.
    DATA mv_object_type TYPE zsat_entity_type.
    DATA mv_mode TYPE string.

    DATA ms_field_info TYPE zsat_adt_element_info.
    DATA: mf_search_calc_fields TYPE string,
          mf_search_db_views    TYPE string.

    "! <p class="shorttext synchronized" lang="en">Reads where-used references for field</p>
    METHODS get_field_where_used.
    "! <p class="shorttext synchronized" lang="en">Reads hierarchy of field</p>
    METHODS get_field_hierarchy.
    "! <p class="shorttext synchronized" lang="en">Fill where used in CDS information</p>
    METHODS fill_where_used_in_cds
      IMPORTING
        iv_base_table  TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type.
    "! <p class="shorttext synchronized" lang="en">Fill where used in DB View</p>
    METHODS fill_where_used_in_view
      IMPORTING
        iv_base_table  TYPE zsat_entity_id
        iv_entity_type TYPE zsat_i_databaseentitywotext-type.
ENDCLASS.



CLASS zcl_sat_adt_res_dbfield_hier IMPLEMENTATION.

  METHOD get.
    mv_object_name = zcl_sat_adt_res_util=>get_request_param_value(
      iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-name
      if_mandatory     = abap_true
      io_request       = request
    ).
    mv_object_type = zcl_sat_adt_res_util=>get_request_param_value(
      iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-type
      io_request       = request
    ).
    mv_field = zcl_sat_adt_res_util=>get_request_param_value(
      iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-field
      if_mandatory     = abap_true
      io_request       = request
    ).
    mv_mode = zcl_sat_adt_res_util=>get_request_param_value(
      iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-mode
      if_mandatory     = abap_true
      io_request       = request
    ).

    IF mv_mode = c_field_mode-hierarchy.
      get_field_hierarchy( ).
    ELSEIF mv_mode = c_field_mode-where_used.
*.... Retrieve additional parameters which are only relevant for the where used query
      mf_search_db_views = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_db_views
        iv_default_value = abap_false
        io_request       = request
      ).
      mf_search_calc_fields = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_calc_fields
        iv_default_value = abap_false
        io_request       = request
      ).
      mv_object_name = to_upper( mv_object_name ).
      mv_field = to_upper( mv_field ).
*.... Finds where-used list for fields
      get_field_where_used( ).
    ENDIF.

    CHECK ms_field_info IS NOT INITIAL.

    response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_generic_eleminfo_res_ch( )
        data            = ms_field_info
    ).
  ENDMETHOD.

  METHOD get_field_hierarchy.
    DATA(lo_hierarchy_resolver) = NEW zcl_sat_cds_field_hier_res( ).
    ms_field_info = lo_hierarchy_resolver->resolve_field_hierarchy(
        iv_cds_view       = mv_object_name
        iv_cds_view_field = mv_field
    ).
  ENDMETHOD.

  METHOD get_field_where_used.
    DATA: lt_type_parts TYPE string_table.
    FIELD-SYMBOLS: <lt_where_used> TYPE zsat_adt_element_info_t.

    DATA(lv_base_table) = mv_object_name.

    SELECT SINGLE type
      FROM zsat_i_databaseentitywotext
      WHERE entity = @lv_base_table
    INTO @DATA(lv_entity_type).

    IF lv_entity_type = zif_sat_c_entity_type=>cds_view.
      SELECT SINGLE viewname
        FROM zsat_p_cdsviewbase
        WHERE entityid = @lv_base_table
      INTO @lv_base_table.
    ENDIF.

    fill_where_used_in_cds( iv_base_table  = lv_base_table
                            iv_entity_type = lv_entity_type ).
    IF mf_search_db_views = abap_true.
      fill_where_used_in_view( iv_base_table  = lv_base_table
                               iv_entity_type = lv_entity_type ).
    ENDIF.

    CHECK ms_field_info-children IS BOUND.

    ASSIGN ms_field_info-children->* TO <lt_where_used>.
    SORT <lt_where_used> BY name.

    LOOP AT <lt_where_used> ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      CHECK <ls_where_used>-type IS NOT INITIAL.
      SPLIT <ls_where_used>-type AT '/' INTO TABLE lt_type_parts.
      CHECK lines( lt_type_parts ) = 2.
      <ls_where_used>-uri = zcl_sat_adt_util=>get_adt_object_ref_uri(
        iv_name = CONV #( <ls_where_used>-name )
        is_type = VALUE #( objtype_tr = lt_type_parts[ 1 ] subtype_wb = lt_type_parts[ 2 ] )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_where_used_in_cds.
    DATA: lt_where_used TYPE zif_sat_ty_adt_types=>ty_t_field_usage.

    SELECT *
      FROM zsat_i_dbfieldusedincdsview( p_basetable = @iv_base_table, p_basefield = @mv_field )
    INTO CORRESPONDING FIELDS OF TABLE @lt_where_used.

    IF mf_search_calc_fields = abap_true.
      DATA(lo_calc_field_usage) = NEW zcl_sat_adt_cds_field_usage(
        iv_cds_view = mv_object_name
        iv_field    = mv_field
      ).
      lt_where_used = VALUE #( BASE lt_where_used ( LINES OF lo_calc_field_usage->get_usages_in_calc_fields( ) ) ).
    ENDIF.

    CHECK lt_where_used IS NOT INITIAL.

    SORT lt_where_used BY entityid fieldname.

    ms_field_info-name = mv_object_name.
    ms_field_info-type = zif_sat_c_adt_utils=>c_adt_types-data_definition.
    DATA(lr_fields) = NEW zsat_adt_element_info_t( ).
    ms_field_info-children = lr_fields.

    LOOP AT lt_where_used ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      APPEND INITIAL LINE TO lr_fields->* ASSIGNING FIELD-SYMBOL(<ls_field>).
      <ls_field>-name = <ls_where_used>-ddlname.
      <ls_field>-raw_name = <ls_where_used>-entityid.
      <ls_field>-type = zif_sat_c_adt_utils=>c_adt_types-data_definition.
      <ls_field>-properties = VALUE #(
        ( key = c_field_name  value = <ls_where_used>-fieldname )
        ( key = c_source_type  value = <ls_where_used>-sourcetype )
      ).
      IF <ls_where_used>-apistate IS NOT INITIAL.
        <ls_field>-properties = VALUE #( BASE <ls_field>-properties
          ( key = c_api_state value = <ls_where_used>-apistate )
        ).
      ENDIF.
      IF <ls_where_used>-is_calculated = abap_true.
        <ls_field>-properties = VALUE #( BASE <ls_field>-properties
          ( key = c_is_calculated value = <ls_where_used>-is_calculated )
        ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_where_used_in_view.
    DATA: lr_fields TYPE REF TO zsat_adt_element_info_t.

    SELECT *
      FROM zsat_i_dbfieldusedinview( p_basetable = @iv_base_table, p_basefield = @mv_field )
      ORDER BY viewname, viewfield
    INTO TABLE @DATA(lt_where_used).

    CHECK lt_where_used IS NOT INITIAL.

    IF ms_field_info-children IS NOT BOUND.
      ms_field_info-name = mv_object_name.
      ms_field_info-type = zif_sat_c_adt_utils=>c_adt_types-data_definition.
      lr_fields = NEW #( ).
      ms_field_info-children = lr_fields.
    ELSE.
      lr_fields = CAST #( ms_field_info-children ).
    ENDIF.

    LOOP AT lt_where_used ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      APPEND INITIAL LINE TO lr_fields->* ASSIGNING FIELD-SYMBOL(<ls_field>).
      <ls_field>-name = <ls_where_used>-viewname.
      <ls_field>-raw_name = <ls_where_used>-viewname.
      <ls_field>-type = zif_sat_c_adt_utils=>c_adt_types-view_definition.
      <ls_field>-properties = VALUE #(
        ( key = c_field_name  value = <ls_where_used>-viewfield )
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
