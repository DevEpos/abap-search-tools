"! <p class="shorttext synchronized">Custom CDS Parser</p>
CLASS zcl_sat_adt_cds_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_datasource_element,
        name         TYPE string,
        alias        TYPE string,
        type         TYPE string,
        sql_relation TYPE string,
        entity_type  TYPE zsat_entity_type,
        description  TYPE string,
        package      TYPE string,
        owner        TYPE string,
      END OF ty_s_datasource_element.
    TYPES ty_t_datasource_element TYPE STANDARD TABLE OF ty_s_datasource_element WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_datasource_part,
        kind     TYPE string,
        elements TYPE ty_t_datasource_element,
      END OF ty_s_datasource_part.
    TYPES ty_t_datasource_part TYPE STANDARD TABLE OF ty_s_datasource_part WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_dependency_tree_property,
        type        TYPE string VALUE 'TYPE',
        alias       TYPE string VALUE 'ALIAS',
        entity_name TYPE string VALUE 'ENTITY_NAME',
        relation    TYPE string VALUE 'RELATION',
      END OF c_dependency_tree_property.
    CONSTANTS:
      BEGIN OF c_sql_relation,
        from             TYPE string VALUE 'FROM',
        association      TYPE string VALUE 'ASSOCIATION',
        inner_join       TYPE string VALUE 'INNER_JOIN',
        left_outer_join  TYPE string VALUE 'LEFT_OUTER_JOIN',
        right_outer_join TYPE string VALUE 'RIGHT_OUTER_JOIN',
        full_outer_join  TYPE string VALUE 'FULL_OUTER_JOIN',
        cross_join       TYPE string VALUE 'CROSS_JOIN',
      END OF c_sql_relation.
    CONSTANTS:
      BEGIN OF c_node_type,
        associations TYPE string VALUE 'ASSOCIATIONS',
        select       TYPE string VALUE 'SELECT',
        result       TYPE string VALUE 'RESULT',
        union        TYPE string VALUE 'UNION',
        union_all    TYPE string VALUE 'UNION_ALL',
      END OF c_node_type.

    DATA ms_result   TYPE zif_sat_ty_adt_types=>ty_cds_top_down_result READ-ONLY.
    DATA mv_cds_view TYPE zsat_cds_view_name                           READ-ONLY.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_cds TYPE zsat_cds_view_name.

    "! <p class="shorttext synchronized">Parse CDS View</p>
    METHODS parse_cds
      IMPORTING
        if_select_part  TYPE abap_bool DEFAULT abap_true
        if_associations TYPE abap_bool OPTIONAL.

  PRIVATE SECTION.
    DATA mo_interpreter TYPE REF TO lcl_ddl_stmnt_interpreter.

    "! <p class="shorttext synchronized">Fills ADT URIs</p>
    METHODS fill_adt_uris
      IMPORTING
        io_node_helper TYPE REF TO lcl_node_helper.

    "! <p class="shorttext synchronized">Fill data source descriptions</p>
    METHODS fill_datasource_information
      IMPORTING
        io_node_helper TYPE REF TO lcl_node_helper.

    "! <p class="shorttext synchronized">Fill CDS view descriptions</p>
    METHODS fill_cds_view_info
      IMPORTING
        io_node_helper TYPE REF TO lcl_node_helper.

    "! <p class="shorttext synchronized">Fill table descriptions</p>
    METHODS fill_table_info
      IMPORTING
        io_node_helper TYPE REF TO lcl_node_helper.

    "! <p class="shorttext synchronized">Fill view descriptions</p>
    METHODS fill_view_info
      IMPORTING
        io_node_helper TYPE REF TO lcl_node_helper .

ENDCLASS.


CLASS zcl_sat_adt_cds_parser IMPLEMENTATION.
  METHOD constructor.
    mv_cds_view = iv_cds.
  ENDMETHOD.

  METHOD parse_cds.
    DATA lv_ddlname TYPE ddlname.
    DATA lv_entity  TYPE zsat_entity_id.

    " Determine the correct DDL name first
    SELECT SINGLE ddlname,
                  entityid
      FROM zsat_p_cdsviewbase
      WHERE entityid = @mv_cds_view
         OR ddlname  = @mv_cds_view
      INTO (@lv_ddlname, @lv_entity).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    SELECT SINGLE
      FROM ddddlsrc
      FIELDS *
      WHERE ddlname = @lv_ddlname
      INTO @DATA(ls_cds).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lo_parser) = NEW cl_ddl_parser( ).
    try.
    data(lo_stmnt) = lo_parser->parse_ddl(
      EXPORTING
        source                  = ls_cds-source
*        bitset                  = 0
*        version                 = 0
*        trace                   = ABAP_FALSE
*      IMPORTING
*        tracestr                =
*        xmlstr                  =
*        errors                  =
    ).
      CATCH cx_ddl_parser_exception.    "
      ENDTRY.

    DATA(lo_node_helper) = NEW lcl_node_helper( iv_name        = |{ lv_entity }|
                                                iv_entity_name = |{ lv_entity }| ).

    IF lo_stmnt IS BOUND.
      CASE lo_stmnt->get_type( ).

        WHEN cl_qlast_constants=>ddlstmt_type_view_definition.
          mo_interpreter = NEW lcl_ddl_view_stmnt_intrpt( if_associations = if_associations
                                                          io_node_helper  = lo_node_helper
                                                          io_stmnt        = CAST cl_qlast_view_definition( lo_stmnt ) ).

      ENDCASE.

    ENDIF.

    IF mo_interpreter IS BOUND.
      mo_interpreter->interpret( ).
      fill_datasource_information( lo_node_helper ).
      fill_adt_uris( lo_node_helper ).
      ms_result = lo_node_helper->convert_to_adt_result( lo_node_helper->mo_root_node ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_adt_uris.
    TYPES:
      BEGIN OF lty_uri,
        name        TYPE zsat_entity_id,
        ddlname     TYPE ddlname,
        entity_type TYPE zsat_entity_type,
      END OF lty_uri.
    DATA lt_uri TYPE STANDARD TABLE OF lty_uri.

    FIELD-SYMBOLS <ls_node> TYPE lcl_node=>ty_s_cached_node.

    lt_uri = VALUE #(
        ( LINES OF VALUE #( FOR cds IN io_node_helper->mt_cds_views
                            ( name = cds-name entity_type = cds-node->entity_type ddlname = cds-node->ddls_name ) ) )
        ( LINES OF VALUE #( FOR table IN io_node_helper->mt_tables ( name = table-name entity_type = table-node->entity_type ) ) )
        ( LINES OF VALUE #( FOR view IN io_node_helper->mt_views ( name = view-name entity_type = view-node->entity_type ) ) ) ).

    SORT lt_uri BY name.
    DELETE ADJACENT DUPLICATES FROM lt_uri COMPARING name.

    LOOP AT lt_uri INTO DATA(ls_uri).
      DATA(ls_adt_object) = zcl_sat_adt_util=>create_adt_uri( iv_type  = ls_uri-entity_type
                                                              iv_name  = ls_uri-name
                                                              iv_name2 = |{ ls_uri-ddlname }| ).
      IF ls_uri-entity_type = zif_sat_c_entity_type=>cds_view.
        LOOP AT io_node_helper->mt_cds_views ASSIGNING <ls_node> WHERE name = ls_uri-name.
          <ls_node>-node->uri      = ls_adt_object-uri.
          <ls_node>-node->adt_type = ls_adt_object-type.
        ENDLOOP.
      ELSE.
        LOOP AT io_node_helper->mt_tables ASSIGNING <ls_node> WHERE name = ls_uri-name.
          <ls_node>-node->uri      = ls_adt_object-uri.
          <ls_node>-node->adt_type = ls_adt_object-type.
        ENDLOOP.

        LOOP AT io_node_helper->mt_views ASSIGNING <ls_node> WHERE name = ls_uri-name.
          <ls_node>-node->uri      = ls_adt_object-uri.
          <ls_node>-node->adt_type = ls_adt_object-type.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_datasource_information.
    fill_cds_view_info( io_node_helper ).
    fill_table_info( io_node_helper ).
    fill_view_info( io_node_helper ).
  ENDMETHOD.

  METHOD fill_cds_view_info.
    DATA lt_cds_range TYPE RANGE OF ddstrucobjname.

    CHECK io_node_helper->mt_cds_views IS NOT INITIAL.

    lt_cds_range = VALUE #( FOR cds IN io_node_helper->mt_cds_views ( sign = 'I' option = 'EQ' low = cds-name ) ).

    SELECT entityid,
           rawentityid,
           ddlname,
           viewname,
           sourcetype AS source_type,
           developmentpackage,
           createdby,
           description
      FROM zsat_i_cdsentity( p_language = @sy-langu )
      WHERE entityid IN @lt_cds_range
         OR ddlname IN @lt_cds_range
      INTO TABLE @DATA(lt_entity_and_text).

    LOOP AT io_node_helper->mt_cds_views ASSIGNING FIELD-SYMBOL(<ls_node>).
      ASSIGN lt_entity_and_text[ ddlname = <ls_node>-name ] TO FIELD-SYMBOL(<ls_entity_info>).
      IF sy-subrc <> 0.
        ASSIGN lt_entity_and_text[ entityid = <ls_node>-name ] TO <ls_entity_info>.
      ENDIF.
      CHECK sy-subrc = 0.
      <ls_node>-name = <ls_entity_info>-entityid.
      <ls_node>-node->entity_name     = <ls_entity_info>-entityid.
      <ls_node>-node->name            = <ls_entity_info>-entityid.
      <ls_node>-node->raw_entity_name = <ls_entity_info>-rawentityid.
      <ls_node>-node->ddls_name = <ls_entity_info>-ddlname.
      <ls_node>-node->source_type = <ls_entity_info>-source_type.
      <ls_node>-node->description = <ls_entity_info>-description.
      <ls_node>-node->owner = <ls_entity_info>-createdby.
      <ls_node>-node->package = <ls_entity_info>-developmentpackage.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_table_info.
    DATA lt_tabname_range TYPE RANGE OF tabname.

    CHECK io_node_helper->mt_tables IS NOT INITIAL.

    lt_tabname_range = VALUE #( FOR table IN io_node_helper->mt_tables ( sign = 'I' option = 'EQ' low = table-name ) ).

    SELECT tablename AS entityid,
           tablename AS rawentityid,
           developmentpackage,
           createdby,
           description
     FROM zsat_i_databasetable( p_language = @sy-langu )
     WHERE tablename IN @lt_tabname_range
    INTO TABLE @DATA(lt_entity_and_text).

    LOOP AT io_node_helper->mt_tables ASSIGNING FIELD-SYMBOL(<ls_node>).
      ASSIGN lt_entity_and_text[ entityid = <ls_node>-name ] TO FIELD-SYMBOL(<ls_entity_info>).
      CHECK sy-subrc = 0.
      <ls_node>-node->description = <ls_entity_info>-description.
      <ls_node>-node->owner       = <ls_entity_info>-createdby.
      <ls_node>-node->package     = <ls_entity_info>-developmentpackage.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_view_info.
    DATA lt_view_range TYPE RANGE OF viewname.

    FIELD-SYMBOLS <ls_node> TYPE lcl_node=>ty_s_cached_node.

    lt_view_range = VALUE #( FOR view IN io_node_helper->mt_views ( sign = 'I' option = 'EQ' low = view-name ) ).

    " Determine view information
    IF lt_view_range IS INITIAL.
      RETURN.
    ENDIF.

    SELECT entityid,
           rawentityid,
           ddlname,
           viewname,
           sourcetype AS source_type,
           developmentpackage,
           createdby,
           description
      FROM zsat_i_cdsentity( p_language = @sy-langu )
      WHERE viewname IN @lt_view_range
      INTO TABLE @DATA(lt_entity_and_text).

    LOOP AT io_node_helper->mt_views ASSIGNING <ls_node>.
      ASSIGN lt_entity_and_text[ viewname = <ls_node>-name ] TO FIELD-SYMBOL(<ls_entity_info>).
      CHECK sy-subrc = 0.
      <ls_node>-name = <ls_entity_info>-entityid.
      <ls_node>-node->entity_name     = <ls_entity_info>-entityid.
      <ls_node>-node->name            = <ls_entity_info>-entityid.
      <ls_node>-node->raw_entity_name = <ls_entity_info>-rawentityid.
      <ls_node>-node->entity_type = zif_sat_c_entity_type=>cds_view.
      <ls_node>-node->ddls_name = <ls_entity_info>-ddlname.
      <ls_node>-node->source_type = <ls_entity_info>-source_type.
      <ls_node>-node->description = <ls_entity_info>-description.
      <ls_node>-node->owner = <ls_entity_info>-createdby.
      <ls_node>-node->package = <ls_entity_info>-developmentpackage.
*.... move the node to the CDS view tables
      io_node_helper->mt_cds_views = VALUE #( BASE io_node_helper->mt_cds_views
                                              ( name = <ls_entity_info>-entityid node = <ls_node>-node ) ).
      DELETE io_node_helper->mt_views.
    ENDLOOP.

    " Fill datasource information for remaining 'true' database views
    IF io_node_helper->mt_views IS INITIAL.
      RETURN.
    ENDIF.
    lt_view_range = VALUE #( FOR view IN io_node_helper->mt_views ( sign = 'I' option = 'EQ' low = view-name ) ).

    SELECT viewname AS entityid,
           viewname AS rawentityid,
           developmentpackage,
           createdby,
           description
      FROM zsat_i_databaseview( p_language = @sy-langu )
      WHERE viewname IN @lt_view_range
      INTO CORRESPONDING FIELDS OF TABLE @lt_entity_and_text.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT io_node_helper->mt_views ASSIGNING <ls_node>.
      ASSIGN lt_entity_and_text[ viewname = <ls_node>-name ] TO <ls_entity_info>.
      CHECK sy-subrc = 0.
      <ls_node>-node->name            = <ls_entity_info>-entityid.
      <ls_node>-node->raw_entity_name = <ls_entity_info>-rawentityid.
      <ls_node>-node->description     = <ls_entity_info>-description.
      <ls_node>-node->owner           = <ls_entity_info>-createdby.
      <ls_node>-node->package         = <ls_entity_info>-developmentpackage.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
