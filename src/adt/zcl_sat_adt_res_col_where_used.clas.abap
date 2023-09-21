"! <p class="shorttext synchronized">Resource for getting Where-Used List of Column</p>
CLASS zcl_sat_adt_res_col_where_used DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_column_info
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS internal_get   REDEFINITION.
    METHODS get_parameters REDEFINITION.

  PRIVATE SECTION.
    DATA mf_search_calc_fields TYPE abap_bool.
    DATA mf_search_db_views    TYPE abap_bool.

    "! <p class="shorttext synchronized">Fill where used in CDS information</p>
    METHODS fill_where_used_in_cds
      IMPORTING
        iv_base_table  TYPE zsat_entity_id
        iv_entity_type TYPE zsat_entity_type.

    "! <p class="shorttext synchronized">Fill where used in DB View</p>
    METHODS fill_where_used_in_view
      IMPORTING
        iv_base_table  TYPE zsat_entity_id
        iv_entity_type TYPE zsat_i_databaseentitywotext-type.
ENDCLASS.


CLASS zcl_sat_adt_res_col_where_used IMPLEMENTATION.
  METHOD fill_where_used_in_cds.
    DATA lt_where_used TYPE zif_sat_ty_adt_types=>ty_t_field_usage.

    SELECT *
      FROM zsat_i_dbfieldusedincdsview( p_basetable = @iv_base_table, p_basefield = @mv_field )
    INTO CORRESPONDING FIELDS OF TABLE @lt_where_used.

    IF mf_search_calc_fields = abap_true.
      DATA(lo_calc_field_usage) = NEW zcl_sat_adt_cds_field_usage( iv_cds_view = mv_object_name
                                                                   iv_field    = mv_field ).
      lt_where_used = VALUE #( BASE lt_where_used ( LINES OF lo_calc_field_usage->get_usages_in_calc_fields( ) ) ).
    ENDIF.

    IF lt_where_used IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_where_used BY entityid
                          fieldname.

    LOOP AT lt_where_used ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      ms_field_info-field_infos = VALUE #( BASE ms_field_info-field_infos
                                           ( field           = <ls_where_used>-fieldname
                                             entity_name     = <ls_where_used>-ddlname
                                             alt_entity_name = <ls_where_used>-entityid
                                             type            = zif_sat_c_object_types=>data_definition
                                             is_calculated   = <ls_where_used>-is_calculated
                                             source_type     = <ls_where_used>-sourcetype ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_where_used_in_view.
    SELECT *
      FROM zsat_i_dbfieldusedinview( p_basetable = @iv_base_table, p_basefield = @mv_field )
      ORDER BY viewname, viewfield
      INTO TABLE @DATA(lt_where_used).

    LOOP AT lt_where_used ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      ms_field_info-field_infos = VALUE #( BASE ms_field_info-field_infos
                                           ( field           = <ls_where_used>-viewfield
                                             entity_name     = <ls_where_used>-viewname
                                             alt_entity_name = <ls_where_used>-viewname
                                             type            = zif_sat_c_object_types=>view_definition ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_parameters.
    super->get_parameters( io_request ).
    mv_object_name = to_upper( mv_object_name ).
    mv_field = to_upper( mv_field ).

    " Retrieve additional parameters which are only relevant for the where used query
    mf_search_db_views = zcl_sat_adt_res_util=>get_request_param_value(
                             iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_db_views
                             iv_default_value = abap_false
                             io_request       = io_request ).
    mf_search_calc_fields = zcl_sat_adt_res_util=>get_request_param_value(
                                iv_param_name    = zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_calc_fields
                                iv_default_value = abap_false
                                io_request       = io_request ).
  ENDMETHOD.

  METHOD internal_get.
    DATA lt_type_parts TYPE string_table.

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

    SORT ms_field_info-field_infos BY entity_name
                                      field.

    LOOP AT ms_field_info-field_infos ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      CHECK <ls_where_used>-type IS NOT INITIAL.
      SPLIT <ls_where_used>-type AT '/' INTO TABLE lt_type_parts.
      CHECK lines( lt_type_parts ) = 2.
      <ls_where_used>-uri = zcl_sat_adt_util=>get_adt_object_ref_uri(
                                iv_name = CONV #( <ls_where_used>-entity_name )
                                is_type = VALUE #( objtype_tr = lt_type_parts[ 1 ] subtype_wb = lt_type_parts[ 2 ] ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
