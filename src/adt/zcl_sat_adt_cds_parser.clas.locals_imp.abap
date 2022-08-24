*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_node_helper IMPLEMENTATION.

  METHOD constructor.
    mo_current_node = NEW #( ).
    mo_current_node->entity_type = zif_sat_c_entity_type=>cds_view.
    mo_current_node->type = 'CDS_VIEW'.
    mo_current_node->name = iv_name.
    mo_current_node->entity_name = iv_entity_name.
    mo_current_node->raw_entity_name = iv_raw_entity_name.
    mo_root_node = mo_current_node.
    mt_cds_views = VALUE #( ( name = iv_entity_name node = mo_current_node ) ).
  ENDMETHOD.


  METHOD get_relation.
    rv_sql_relation = COND string(
      WHEN NOT io_parent_node->children IS NOT INITIAL               THEN zcl_sat_adt_cds_parser=>c_sql_relation-from
      WHEN iv_datasource_type = cl_qlast_constants=>datasource_inner THEN zcl_sat_adt_cds_parser=>c_sql_relation-inner_join
      WHEN iv_datasource_type = cl_qlast_constants=>datasource_left  THEN zcl_sat_adt_cds_parser=>c_sql_relation-left_outer_join
      WHEN iv_datasource_type = cl_qlast_constants=>datasource_right THEN zcl_sat_adt_cds_parser=>c_sql_relation-right_outer_join
      WHEN iv_datasource_type = cl_qlast_constants=>datasource_cross THEN zcl_sat_adt_cds_parser=>c_sql_relation-cross_join
      WHEN iv_datasource_type = cl_qlast_constants=>datasource_full  THEN zcl_sat_adt_cds_parser=>c_sql_relation-full_outer_join ).
  ENDMETHOD.

  METHOD add_child.


    DATA(lo_child) = NEW lcl_node( ).
    lo_child->name = iv_name.
    lo_child->entity_type = iv_entity_type.
    lo_child->type = COND #(
      WHEN iv_entity_type = zif_sat_c_entity_type=>cds_view THEN 'CDS_VIEW'
      WHEN iv_entity_type = zif_sat_c_entity_type=>table    THEN 'TABLE'
      WHEN iv_entity_type = zif_sat_c_entity_type=>view     THEN 'VIEW'
      ELSE iv_type ).
    lo_child->relation = iv_relation.
    lo_child->alias = iv_alias.
    lo_child->parent = mo_current_node.

    IF iv_entity_type = zif_sat_c_entity_type=>view.
      mt_views = VALUE #( BASE mt_views ( name = lo_child->name node = lo_child ) ).
    ELSEIF iv_entity_type = zif_sat_c_entity_type=>table.
      mt_tables = VALUE #( BASE mt_tables ( name = lo_child->name node = lo_child ) ).
    ELSEIF iv_entity_type = zif_sat_c_entity_type=>cds_view.
      mt_cds_views = VALUE #( BASE mt_cds_views ( name = lo_child->name node = lo_child ) ).
    ENDIF.

    IF io_parent_node->children IS INITIAL.
      io_parent_node->children = VALUE #( ).
    ENDIF.
    io_parent_node->children = VALUE #( BASE io_parent_node->children ( lo_child ) ).

    ro_added = lo_child.
  ENDMETHOD.

  METHOD set_parent_as_current_node.
    IF mo_current_node->parent IS BOUND.
      mo_current_node = mo_current_node->parent.
    ENDIF.
  ENDMETHOD.

  METHOD current_parent_has_children.
    rf_has_children = xsdbool( mo_current_node->children IS NOT INITIAL ).
  ENDMETHOD.

  METHOD fill_element_info_from_node.
    cs_elem_info-name = io_node->name.
    cs_elem_info-raw_name = COND #( WHEN io_node->raw_entity_name IS NOT INITIAL THEN io_node->raw_entity_name ELSE io_node->name ).
    cs_elem_info-entity_type = io_node->entity_type.
    cs_elem_info-type = io_node->adt_type.
    cs_elem_info-package = io_node->package.
    cs_elem_info-description = io_node->description.
    cs_elem_info-uri = io_node->uri.
    cs_elem_info-owner = io_node->owner.
*.. Fill properties
    IF io_node->relation IS NOT INITIAL.
      cs_elem_info-properties = VALUE #( BASE cs_elem_info-properties
        ( key   = zcl_sat_adt_cds_parser=>c_dependency_tree_property-relation
          value = io_node->relation ) ).
    ENDIF.
    IF io_node->alias IS NOT INITIAL AND io_node->alias NP '=*'.
      cs_elem_info-properties = VALUE #( BASE cs_elem_info-properties
        ( key   = zcl_sat_adt_cds_parser=>c_dependency_tree_property-alias
          value = io_node->alias ) ).
    ENDIF.
    IF io_node->api_state IS NOT INITIAL.
      cs_elem_info-properties = VALUE #( BASE cs_elem_info-properties
        ( key   = 'API_STATE'
          value = io_node->api_state ) ).
    ENDIF.
    IF io_node->source_type IS NOT INITIAL.
      cs_elem_info-properties = VALUE #( BASE cs_elem_info-properties
        ( key   = 'SOURCE_TYPE'
          value = io_node->source_type ) ).
    ENDIF.
    IF io_node->name2 IS NOT INITIAL AND io_node->relation = zcl_sat_adt_cds_parser=>c_sql_relation-association.
      cs_elem_info-properties = VALUE #( BASE cs_elem_info-properties
        ( key   = 'ASSOCIATION_NAME'
          value = io_node->name2 ) ).
    ENDIF.

    cs_elem_info-properties = VALUE #( BASE cs_elem_info-properties
      ( key   = zcl_sat_adt_cds_parser=>c_dependency_tree_property-type
        value = io_node->type ) ).
  ENDMETHOD.

  METHOD convert_node_to_elem_info.
    FIELD-SYMBOLS: <lt_children> TYPE zsat_adt_element_info_t.

    fill_element_info_from_node( EXPORTING io_node = io_node CHANGING cs_elem_info = cs_elem_info ).

    IF io_node->children IS NOT INITIAL.
      cs_elem_info-children = NEW zsat_adt_element_info_t( ).
      ASSIGN cs_elem_info-children->* TO <lt_children>.
    ENDIF.

    LOOP AT io_node->children INTO DATA(lo_child).
      APPEND INITIAL LINE TO <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child_eleminfo>).
      convert_node_to_elem_info(
        EXPORTING
          io_node      = lo_child
        CHANGING
          cs_elem_info = <ls_child_eleminfo> ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ddl_stmnt_interpreter IMPLEMENTATION.
  METHOD constructor.
    mo_node_helper = io_node_helper.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_ddl_view_stmnt_intrpt IMPLEMENTATION.

  METHOD constructor.
    super->constructor( io_node_helper ).
    mo_stmnt = io_stmnt.
    mf_associations = if_associations.
  ENDMETHOD.

  METHOD interpret.
    DATA(lo_select) = get_root_select( ).
    IF lo_select IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_parent_node) = mo_node_helper->mo_root_node.

    IF lo_select->get_union( ) IS NOT INITIAL.
      lo_parent_node = mo_node_helper->add_child(
        io_parent_node = lo_parent_node
        iv_name        = zcl_sat_adt_cds_parser=>c_node_type-select
        iv_type        = zcl_sat_adt_cds_parser=>c_node_type-select ).
    ENDIF.

    interpret_select_stmnt(
      io_parent_node = lo_parent_node
      io_select      = lo_select ).

*.. Retrieve associations if requested
    get_associations(
      io_parent_node = mo_node_helper->mo_root_node
      io_select      = lo_select ).
  ENDMETHOD.

  METHOD interpret_select_stmnt.
    CHECK io_select IS BOUND.

    DATA(lo_from_datasource) = io_select->get_from( ).

*.. Create From sources
    DATA(lo_parent_node) = io_parent_node.
    IF if_union = abap_true.

      DATA(lv_name) = COND #( WHEN if_union_all = abap_true THEN zcl_sat_adt_cds_parser=>c_node_type-union_all
                              ELSE                               zcl_sat_adt_cds_parser=>c_node_type-union ).

      lo_parent_node = mo_node_helper->add_child(
        io_parent_node = io_parent_node->parent
        iv_name        = lv_name
        iv_type        = lv_name ).
    ENDIF.
    interpret_datasource(
      io_parent_node = lo_parent_node
      io_datasource  = lo_from_datasource
      iv_parent_type = lo_from_datasource->get_type( ) ).

    interpret_select_stmnt(
      io_parent_node = io_parent_node
      io_select      = io_select->get_union( )
      if_union       = abap_true
      if_union_all   = io_select->is_union_all( ) ).
  ENDMETHOD.


  METHOD interpret_join.
    interpret_datasource(
      io_parent_node = io_parent_node
      io_datasource  = io_join_datasource->get_left( )
      iv_parent_type = io_join_datasource->get_type( )
    ).

    DATA(lo_right_source) = io_join_datasource->get_right( ).
    DATA(lo_parent_node) = io_parent_node.
    IF lo_right_source->get_type( ) <> cl_qlast_constants=>datasource_table.
      lo_parent_node = mo_node_helper->add_child(
        io_parent_node     = io_parent_node
        iv_name            = zcl_sat_adt_cds_parser=>c_node_type-result
        iv_type            = zcl_sat_adt_cds_parser=>c_node_type-result
        iv_relation        = mo_node_helper->get_relation(
          io_parent_node     = io_parent_node
          iv_datasource_type = io_join_datasource->get_type( ) ) ).
    ENDIF.
    interpret_datasource(
      io_parent_node = lo_parent_node
      io_datasource  = lo_right_source
      iv_parent_type = io_join_datasource->get_type( ) ).

  ENDMETHOD.

  METHOD interpret_datasource.
    CHECK io_datasource IS BOUND.

    CASE io_datasource->get_type( ).
      WHEN cl_qlast_constants=>datasource_table.
        DATA(lo_table_datasource) = CAST cl_qlast_table_datasource( io_datasource ).

        mo_node_helper->add_child(
          io_parent_node = io_parent_node
          iv_name        = lo_table_datasource->get_name( )
          iv_entity_name = lo_table_datasource->get_name( )
          iv_relation    = mo_node_helper->get_relation(
            io_parent_node      = io_parent_node
            iv_datasource_type = iv_parent_type )
          iv_entity_type = SWITCH #( lo_table_datasource->get_tabletype( )
             WHEN cl_qlast_constants=>tabtype_entity OR
                  cl_qlast_constants=>tabtype_view_entity OR
                  cl_qlast_constants=>tabtype_table_function THEN zif_sat_c_entity_type=>cds_view
             WHEN cl_qlast_constants=>tabtype_transparent THEN zif_sat_c_entity_type=>table
             WHEN cl_qlast_constants=>tabtype_view THEN zif_sat_c_entity_type=>view )
          iv_alias = lo_table_datasource->get_alias( upper_case = abap_false ) ).

      WHEN cl_qlast_constants=>datasource_inner OR
           cl_qlast_constants=>datasource_cross OR
           cl_qlast_constants=>datasource_left OR
           cl_qlast_constants=>datasource_right OR
           cl_qlast_constants=>datasource_full.
        interpret_join(
          io_parent_node     = io_parent_node
          io_join_datasource = CAST cl_qlast_join_datasource( io_datasource ) ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_associations.
    CHECK mf_associations = abap_true.

    DATA(lo_associations) = io_select->get_associations( ).
    CHECK lo_associations IS NOT INITIAL.

    DATA(lo_associations_node) = mo_node_helper->add_child(
      io_parent_node = io_parent_node
      iv_name        = zcl_sat_adt_cds_parser=>c_node_type-associations
      iv_type        = zcl_sat_adt_cds_parser=>c_node_type-associations ).

    LOOP AT lo_associations->get_entries( ) INTO DATA(lo_association).
      DATA(lo_target) = lo_association->get_target( ).

      DATA(lo_node) = mo_node_helper->add_child(
        io_parent_node = lo_associations_node
        iv_name        = lo_target->get_name( )
        iv_entity_name = lo_target->get_name( )
        iv_relation    = zcl_sat_adt_cds_parser=>c_sql_relation-association
        iv_entity_type = SWITCH #( lo_target->get_tabletype( )
           WHEN cl_qlast_constants=>tabtype_entity OR
                cl_qlast_constants=>tabtype_view_entity OR
                cl_qlast_constants=>tabtype_table_function THEN zif_sat_c_entity_type=>cds_view
           WHEN cl_qlast_constants=>tabtype_transparent THEN zif_sat_c_entity_type=>table
           WHEN cl_qlast_constants=>tabtype_view THEN zif_sat_c_entity_type=>view ) ).
      lo_node->name2 = lo_association->get_name( upper_case = abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_root_select.
    ro_select = mo_stmnt->get_select( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ddl_tab_func_stmnt_intrpt IMPLEMENTATION.

  METHOD constructor.
    super->constructor( io_node_helper ).
    mo_stmnt = io_stmnt.
  ENDMETHOD.

  METHOD interpret.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_ddl_view2_stmnt_intrpt IMPLEMENTATION.

  METHOD get_root_select.
    IF mo_stmnt->get_query( ) IS INSTANCE OF cl_qlast_select.
      ro_select = CAST #( mo_stmnt->get_query( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor(
      if_associations = if_associations
      io_node_helper  = io_node_helper
      io_stmnt        = io_stmnt ).
    mo_stmnt = io_stmnt.
  ENDMETHOD.

ENDCLASS.
