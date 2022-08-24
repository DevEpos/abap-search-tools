*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_parser IMPLEMENTATION.

  METHOD constructor.
    me->ddls = ddls.
  ENDMETHOD.

  METHOD parse.
    DATA(parser) = NEW cl_ddl_parser( ).
    DATA(stmnt) = parser->parse_cds(
      it_sources   = VALUE #( ( ddls ) )
      iv_bitset    = cl_ddl_parser=>set_bitmask(
        iv_ars_check_off = abap_true
        iv_extresol      = abap_true ) ).

    DATA(visitor) = NEW lcl_field_visitor(
      source_entityname = |{ ddls-ddlname }| ).
    IF stmnt IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        stmnt->accept( visitor ).
        DATA(l_fields) = visitor->get_found_fields( ).

        basetables = CORRESPONDING #( l_fields DISCARDING DUPLICATES ).
        fields = CORRESPONDING #( l_fields ).
      CATCH cx_ddl_visitor_exception.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_field_visitor IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    me->source_entityname = source_entityname.

    m_mapping = VALUE #(
      ( classname = 'CL_QLAST_STDSELECTLIST_ENTRY'   method = 'VISIT_STDSELECTLIST_ENTRY' )
      ( classname = 'CL_QLAST_ATOMIC_EXPRESSION'     method = 'VISIT_ATOMIC_EXPRESSION' )
      ( classname = 'CL_QLAST_LITERAL_EXPRESSION'    method = 'VISIT_LITERAL_EXPRESSION' ) ).

    ignore_flags = if_qlast_visitor=>bitmask_ignore_associations BIT-OR
      if_qlast_visitor=>bitmask_ignore_path BIT-OR
      if_qlast_visitor=>bitmask_ignore_annotations BIT-OR
      if_qlast_visitor=>bitmask_ignore_group_by BIT-OR
      if_qlast_visitor=>bitmask_ignore_where BIT-OR
      if_qlast_visitor=>bitmask_ignore_from BIT-OR
      if_qlast_visitor=>bitmask_ignore_filter BIT-OR
      if_qlast_visitor=>bitmask_ignore_order_by BIT-OR
      if_qlast_visitor=>bitmask_ignore_ast_base.
  ENDMETHOD.


  METHOD get_found_fields.
    result = found_fields.
  ENDMETHOD.


  METHOD if_qlast_visitor~ignore.
    mask = ignore_flags.
  ENDMETHOD.


  METHOD if_qlast_visitor~after.

    IF object IS INSTANCE OF cl_qlast_stdselectlist_entry.
      IF atomic_field_found = abap_false AND literal_found IS NOT INITIAL.

        INSERT VALUE #(
          ddlname       = source_entityname
          basetable     = COND #( WHEN literal_found-char = abap_true THEN 'DDDDLCHARTYPES' ELSE 'DDDDLDECTYPES' )
          fieldname     = COND #( WHEN literal_found-char = abap_true THEN 'CCHAR1' ELSE 'DEC1_0' )
          viewfield     = source_field
          fieldpos      = field_pos ) INTO TABLE found_fields.

        field_pos = field_pos + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_qlast_visitor~get_descend.
    descend = if_qlast_visitor=>descend_inorder.
  ENDMETHOD.


  METHOD visit_stdselectlist_entry.
    CHECK object IS BOUND.

    CLEAR: literal_found,
           atomic_field_found.

    DATA(entry) = CAST cl_qlast_stdselectlist_entry( object ).
    IF entry->has_alias( ).
      source_field = entry->get_alias( ).
    ELSE.
      source_field = entry->get_name( ).
    ENDIF.
  ENDMETHOD.


  METHOD visit_atomic_expression.
    DATA: datasource_name TYPE string,
          is_cds_v1       TYPE abap_bool.

    CHECK object IS BOUND.

    atomic_field_found = abap_true.

    DATA(atomic_expr) = CAST cl_qlast_atomic_expression( object ).
    DATA(identifier) = atomic_expr->get_identifier( ).
    DATA(datasource) = atomic_expr->get_datasource( ).

    IF datasource IS BOUND.
      datasource_name = datasource->get_name( ).
      is_cds_v1 = xsdbool( datasource->get_tabletype( ) = cl_qlast_constants=>tabtype_entity ).
    ELSE.
      datasource_name = atomic_expr->get_tablename( ).
    ENDIF.

    IF datasource_name IS NOT INITIAL.
      INSERT VALUE #(
        ddlname       = source_entityname
        basetable     = datasource_name
        fieldname     = identifier
        viewfield     = source_field
        fieldpos      = field_pos
        is_cds_v1     = is_cds_v1 ) INTO TABLE found_fields.

      field_pos = field_pos + 1.
    ENDIF.
  ENDMETHOD.


  METHOD visit_literal_expression.
    CHECK: object IS BOUND,
           literal_found IS INITIAL.

    DATA(literal_expr) = CAST cl_qlast_literal_expression( object ).
    literal_found = VALUE #( char = literal_expr->is_string( ) num = literal_expr->is_numeric( ) ).
  ENDMETHOD.

ENDCLASS.
