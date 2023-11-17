"! <p class="shorttext synchronized">CDS Usage Analyzer</p>
CLASS zcl_sat_cds_usage_analyzer DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_qlast_visitor.

    TYPES:
      BEGIN OF ty_metrics,
        occurrences         TYPE i,
        cds_view_count      TYPE i,
        table_func_count    TYPE i,
        view_count          TYPE i,
        table_count         TYPE i,
        join_count          TYPE i,
        "! Union, Except, intersect
        set_operation_count TYPE i,
        group_by_count      TYPE i,
        cast_count          TYPE i,
        case_count          TYPE i,
        function_count      TYPE i,
      END OF ty_metrics.
    TYPES  BEGIN OF ty_dependency.
    TYPES:   name          TYPE zsat_entity_id,
             raw_name      TYPE zsat_entity_id_raw,
             object_type   TYPE zsat_entity_type,
             adt_type      TYPE string,
             uri           TYPE string,
             package       TYPE string,
             description   TYPE ddtext,
             source_type   TYPE char1,
             api_state     TYPE string,
             used_entities TYPE i.
             INCLUDE TYPE ty_metrics AS metrics.
    TYPES  END OF ty_dependency.

    TYPES ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH KEY name object_type.

    METHODS constructor
      IMPORTING
        iv_ddlname TYPE ddlname
        if_adt     TYPE abap_bool OPTIONAL.

    "! Analyses dependencies of a DDL Source
    METHODS analyze_dependencies
      RETURNING
        VALUE(result) TYPE ty_dependencies.

  PRIVATE SECTION.
    TYPES ty_cds_views TYPE SORTED TABLE OF ddlname WITH UNIQUE KEY table_line.
    TYPES ty_base_tables TYPE STANDARD TABLE OF dd26v WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_used_cds,
        name         TYPE ddlname,
        count        TYPE i,
        metrics_used TYPE abap_bool,
      END OF ty_used_cds,

      ty_used_cds_list TYPE SORTED TABLE OF ty_used_cds WITH UNIQUE KEY name.

    TYPES BEGIN OF ty_entity_metrics.
    TYPES   entity_name   TYPE tabname.
    TYPES   entity_type   TYPE zsat_entity_type.
    TYPES   used_entities TYPE ty_used_cds_list.
    TYPES   metrics_final TYPE abap_bool.
            INCLUDE TYPE ty_metrics AS metrics.
    TYPES END OF ty_entity_metrics.

    TYPES:
      BEGIN OF ty_child_to_parent,
        child_entity TYPE zsat_entity_id,
        parent_refs  TYPE TABLE OF REF TO ty_entity_metrics WITH EMPTY KEY,
      END OF ty_child_to_parent.

    TYPES ty_entity_metrics_map TYPE HASHED TABLE OF ty_entity_metrics WITH UNIQUE KEY entity_name entity_type.

    TYPES:
      BEGIN OF ty_cds_view_info,
        ddlname            TYPE zsat_i_cdsentity-ddlname,
        entityid           TYPE zsat_i_cdsentity-entityid,
        rawentityid        TYPE zsat_i_cdsentity-rawentityid,
        sourcetype         TYPE zsat_i_cdsentity-sourcetype,
        apistate           TYPE zsat_i_apistates-apistate,
        description        TYPE zsat_i_cdsentity-description,
        developmentpackage TYPE zsat_i_cdsentity-developmentpackage,
        createdby          TYPE zsat_i_cdsentity-createdby,
      END OF ty_cds_view_info.

    TYPES ty_cds_view_infos TYPE SORTED TABLE OF ty_cds_view_info WITH NON-UNIQUE KEY ddlname.
    TYPES ty_dbtab_infos TYPE SORTED TABLE OF zsat_i_databasetable WITH UNIQUE KEY tablename.
    TYPES ty_dbview_infos TYPE SORTED TABLE OF zsat_i_databaseview WITH UNIQUE KEY viewname.

    DATA mt_entities TYPE ty_entity_metrics_map.
    DATA mt_new_cds_views TYPE ty_cds_views.
    DATA ms_metrics TYPE ty_metrics.
    DATA mv_current_entity TYPE zsat_entity_id.
    DATA mv_ignore TYPE if_qlast_visitor=>bitmask.
    DATA mt_mapping TYPE if_qlast_visitor=>class2method_table.
    DATA mt_used_cds_views TYPE ty_used_cds_list.
    DATA mo_dd_ddl_handler TYPE REF TO if_dd_ddl_handler.
    DATA mo_dd_view_util TYPE REF TO if_dd_view_utilities.
    DATA mv_ddlname TYPE ddlname.
    DATA mf_adt TYPE abap_bool.

    METHODS visit_join_datasource
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_set_operation
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_select
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_table_datasource
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_func_expression
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_case_exp_base
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_cast_expression
      IMPORTING
        !object TYPE REF TO object.

    METHODS visit_groupby
      IMPORTING
        !object TYPE REF TO object.

    METHODS analyze_single_ddls
      IMPORTING
        iv_ddlname TYPE ddlname.

    METHODS update_metrics
      IMPORTING
        iv_ddlname TYPE ddlname.

    METHODS collect_entity
      IMPORTING
        iv_type       TYPE zsat_entity_type
        iv_name       TYPE zsat_entity_id
        iv_joins      TYPE i OPTIONAL
        iv_tables     TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS propagate_metrics.

    METHODS collect_child_metrics
      IMPORTING
        ir_entity_metrics TYPE REF TO ty_entity_metrics.

    METHODS collect_used_cds_view
      IMPORTING
        iv_cds_view TYPE ddlname.

    METHODS process_used_view
      IMPORTING
        iv_view TYPE viewname.

    METHODS get_base_tables
      IMPORTING
        sql_view_name TYPE viewname
      RETURNING
        VALUE(result) TYPE ty_base_tables.

    METHODS get_ddls_name
      IMPORTING
        iv_view_name  TYPE viewname
      RETURNING
        VALUE(result) TYPE ddlname.

    METHODS is_external_view
      IMPORTING
        iv_view_name  TYPE viewname
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS process_used_cds_view
      IMPORTING
        iv_ddlname TYPE ddlname.

    METHODS get_ddlname_for_entity
      IMPORTING
        iv_entity_name TYPE zsat_entity_id
      RETURNING
        VALUE(result)  TYPE ddlname.

    METHODS fill_additional_information
      RETURNING
        VALUE(result) TYPE ty_dependencies.

    METHODS get_cds_infos
      RETURNING
        VALUE(result) TYPE ty_cds_view_infos.

    METHODS get_table_infos
      RETURNING
        VALUE(result) TYPE ty_dbtab_infos.

    METHODS get_view_infos
      RETURNING
        VALUE(result) TYPE ty_dbview_infos.
ENDCLASS.


CLASS zcl_sat_cds_usage_analyzer IMPLEMENTATION.
  METHOD constructor.
    mf_adt = if_adt.
    mt_mapping = VALUE #( ( classname = 'CL_QLAST_SELECT'             method = 'VISIT_SELECT' )
                          ( classname = 'CL_QLAST_TABLE_DATASOURCE'   method = 'VISIT_TABLE_DATASOURCE' )
                          ( classname = 'CL_QLAST_JOIN_DATASOURCE'    method = 'VISIT_JOIN_DATASOURCE' )
                          ( classname = 'CL_QLAST_UNION'              method = 'VISIT_SET_OPERATION' )
                          ( classname = 'CL_QLAST_FUNC_EXPRESSION'    method = 'VISIT_FUNC_EXPRESSION' )
                          ( classname = 'CL_QLAST_STDFUNC_EXPRESSION' method = 'VISIT_FUNC_EXPRESSION' )
                          ( classname = 'CL_QLAST_FUNC_NP_EXPRESSION' method = 'VISIT_FUNC_EXPRESSION' )
                          ( classname = 'CL_QLAST_CASE_EXP_BASE'      method = 'VISIT_CASE_EXP_BASE' )
                          ( classname = 'CL_QLAST_CAST_EXPRESSION'    method = 'VISIT_CAST_EXPRESSION' )
                          ( classname = 'CL_QLAST_GROUPBY'            method = 'VISIT_GROUPBY' ) ).

    mv_ignore = if_qlast_visitor=>bitmask_ignore_associations BIT-OR
                if_qlast_visitor=>bitmask_ignore_path BIT-OR
                if_qlast_visitor=>bitmask_ignore_annotations BIT-OR
                " Not Ignored to count GROUP BY Clause
                " if_qlast_visitor=>bitmask_ignore_group_by BIT-OR
                " Not ignored to collect function calls in WHERE clauses
                " if_qlast_visitor=>bitmask_ignore_where BIT-OR
                if_qlast_visitor=>bitmask_ignore_filter BIT-OR
                if_qlast_visitor=>bitmask_ignore_order_by BIT-OR
                if_qlast_visitor=>bitmask_ignore_ast_base.
    mv_ddlname = iv_ddlname.
  ENDMETHOD.

  METHOD analyze_dependencies.
    DATA lt_temp_views TYPE ty_cds_views.

    lt_temp_views = VALUE #( ( mv_ddlname ) ).

    WHILE lt_temp_views IS NOT INITIAL.
      LOOP AT lt_temp_views INTO DATA(lv_cds).
        analyze_single_ddls( lv_cds ).
      ENDLOOP.

      lt_temp_views = mt_new_cds_views.

      CLEAR mt_new_cds_views.
    ENDWHILE.

    propagate_metrics( ).

    SORT mt_entities BY cds_view_count DESCENDING
                        view_count DESCENDING
                        table_count DESCENDING.

    result = fill_additional_information( ).
  ENDMETHOD.

  METHOD analyze_single_ddls.
    DATA lt_ddlsources TYPE TABLE OF ddddlsrc.

    SELECT *
       FROM ddddlsrc
       WHERE (    ddlname = @iv_ddlname
               OR parentname = @iv_ddlname )
         AND as4local = 'A'
       ORDER BY parentname " The extended view needs to be analyzed first
       INTO TABLE @lt_ddlsources.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: mt_used_cds_views,
           ms_metrics.

    ms_metrics-occurrences = 1.
    mv_current_entity = iv_ddlname.

    DATA(lo_parser) = NEW cl_ddl_parser( ).
    DATA(lo_stmnt) = lo_parser->parse_cds( it_sources = lt_ddlsources
                                           iv_bitset  = cl_ddl_parser=>set_bitmask( iv_semantic = abap_true
                                                                                    iv_trace    = abap_false
                                                                                    iv_locally  = abap_false
                                                                                    iv_aiepp    = abap_false
                                                                                    iv_extresol = abap_true ) ).

    " There could be parsing errors in which case no parsed statement will be returned
    IF lo_stmnt IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_stmnt->accept( me ).
        update_metrics( iv_ddlname = iv_ddlname ).
      CATCH cx_ddl_visitor_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD update_metrics.
    DATA(lr_entity_metrics) = REF #( mt_entities[ entity_name = iv_ddlname
                                                  entity_type = zif_sat_c_entity_type=>cds_view ] OPTIONAL ).

    IF lr_entity_metrics IS INITIAL.
      INSERT VALUE #( entity_name   = iv_ddlname
                      entity_type   = zif_sat_c_entity_type=>cds_view
                      used_entities = mt_used_cds_views
                      metrics       = ms_metrics )
             INTO TABLE mt_entities.
    ELSE.
      lr_entity_metrics->metrics       = CORRESPONDING #( BASE ( lr_entity_metrics->metrics ) ms_metrics EXCEPT occurrences ).
      lr_entity_metrics->used_entities = mt_used_cds_views.
    ENDIF.
  ENDMETHOD.

  METHOD propagate_metrics.
    DATA lr_entity_metrics TYPE REF TO zcl_sat_cds_usage_analyzer=>ty_entity_metrics.

    LOOP AT mt_entities REFERENCE INTO lr_entity_metrics WHERE used_entities IS INITIAL.
      lr_entity_metrics->metrics_final = abap_true.
    ENDLOOP.

    WHILE line_exists( mt_entities[ metrics_final = abap_false ] ).
      LOOP AT mt_entities REFERENCE INTO lr_entity_metrics WHERE metrics_final = abap_false.
        collect_child_metrics( lr_entity_metrics  ).
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.

  METHOD collect_child_metrics.
    LOOP AT ir_entity_metrics->used_entities REFERENCE INTO DATA(lr_used_entity).
      DATA(lr_used_entity_metrics) = REF #( mt_entities[ entity_name = lr_used_entity->name
                                                         entity_type = zif_sat_c_entity_type=>cds_view ] OPTIONAL ).

      IF lr_used_entity_metrics IS NOT BOUND.
        CONTINUE.
      ENDIF.

      IF lr_used_entity_metrics->metrics_final = abap_false.
        RETURN.
      ENDIF.

      IF lr_used_entity->metrics_used = abap_true.
        CONTINUE.
      ENDIF.

      ir_entity_metrics->cds_view_count      = ir_entity_metrics->cds_view_count + lr_used_entity_metrics->cds_view_count * lr_used_entity->count.
      ir_entity_metrics->table_count         = ir_entity_metrics->table_count + lr_used_entity_metrics->table_count * lr_used_entity->count.
      ir_entity_metrics->view_count          = ir_entity_metrics->view_count +  lr_used_entity_metrics->view_count * lr_used_entity->count.
      ir_entity_metrics->table_func_count    = ir_entity_metrics->table_func_count + lr_used_entity_metrics->table_func_count * lr_used_entity->count.
      ir_entity_metrics->set_operation_count = ir_entity_metrics->set_operation_count + lr_used_entity_metrics->set_operation_count * lr_used_entity->count.
      ir_entity_metrics->join_count          = ir_entity_metrics->join_count + lr_used_entity_metrics->join_count * lr_used_entity->count.
      ir_entity_metrics->function_count      = ir_entity_metrics->function_count + lr_used_entity_metrics->function_count * lr_used_entity->count.
      ir_entity_metrics->case_count          = ir_entity_metrics->case_count + lr_used_entity_metrics->case_count * lr_used_entity->count.
      ir_entity_metrics->cast_count          = ir_entity_metrics->cast_count + lr_used_entity_metrics->cast_count * lr_used_entity->count.
      ir_entity_metrics->group_by_count      = ir_entity_metrics->group_by_count + lr_used_entity_metrics->group_by_count * lr_used_entity->count.

      lr_used_entity->metrics_used = abap_true.
    ENDLOOP.

    ir_entity_metrics->metrics_final = abap_true.
  ENDMETHOD.

  METHOD visit_join_datasource.
    ms_metrics-join_count = ms_metrics-join_count + 1.
  ENDMETHOD.

  METHOD visit_set_operation.
    ms_metrics-set_operation_count = ms_metrics-set_operation_count + 1.
*    DATA union_type TYPE i.
*
*    CALL METHOD object->('GET_TYPE')
*      RECEIVING type = union_type.
*
*    CASE union_type.
*      WHEN 1 OR 2. " union
*        ms_metrics-unions = ms_metrics-unions + 1.
*
*      WHEN 2 OR 3. " intersect
*        ms_metrics-intersects = ms_metrics-intersects + 1.
*
*      WHEN 5 OR 6. " except
*        ms_metrics-excepts = ms_metrics-excepts + 1.
*    ENDCASE.
  ENDMETHOD.

  METHOD visit_select.
    DATA(current_select) = CAST cl_qlast_select( object ).

    IF current_select->get_union( ) IS BOUND.
      ms_metrics-set_operation_count = ms_metrics-set_operation_count + 1.
    ENDIF.
  ENDMETHOD.

  METHOD visit_table_datasource.
    DATA(data_source) = CAST cl_qlast_table_datasource( object ).

    CASE data_source->get_tabletype( ).
      WHEN cl_qlast_constants=>tabtype_transparent.
        ms_metrics-table_count = ms_metrics-table_count + 1.
        collect_entity( iv_type = zif_sat_c_entity_type=>table
                        iv_name = CONV #( data_source->get_name( ) ) ).

      WHEN cl_qlast_constants=>tabtype_table_function.
        ms_metrics-table_func_count = ms_metrics-table_func_count + 1.
        collect_entity( iv_type = zif_sat_c_entity_type=>cds_view
                        iv_name = CONV #( data_source->get_name( ) ) ).
      WHEN cl_qlast_constants=>tabtype_entity OR
           'W' OR " cl_qlast_constants=>tabtype_view_entity ( missing on NW 7.54)
           'R' OR " cl_qlast_constants=>tabtype_cds_projection (missing on NW 7.53)
           'H'. " cl_qlast_constants=>tabtype_hierarchy (missing on NW 7.53).
        DATA(lv_ddlname) = get_ddlname_for_entity( CONV #( data_source->get_name( ) ) ).
        process_used_cds_view( lv_ddlname ).

      WHEN cl_qlast_constants=>tabtype_view.
        process_used_view( CONV #( data_source->get_name( ) ) ).
    ENDCASE.
  ENDMETHOD.

  METHOD visit_case_exp_base.
    ms_metrics-case_count = ms_metrics-case_count + 1.
  ENDMETHOD.

  METHOD visit_cast_expression.
    ms_metrics-cast_count = ms_metrics-cast_count + 1.
  ENDMETHOD.

  METHOD visit_func_expression.
    ms_metrics-function_count = ms_metrics-function_count + 1.
  ENDMETHOD.

  METHOD visit_groupby.
    ms_metrics-group_by_count = ms_metrics-group_by_count + 1.
  ENDMETHOD.

  METHOD if_qlast_visitor~call.
    CALL METHOD me->(method)
      EXPORTING object = object.
  ENDMETHOD.

  METHOD if_qlast_visitor~get_descend.
    descend = if_qlast_visitor=>descend_inorder.
  ENDMETHOD.

  METHOD if_qlast_visitor~get_inheritance.
    inheritance = if_qlast_visitor=>inheritance_before_parent.
  ENDMETHOD.

  METHOD if_qlast_visitor~get_mapping.
    mapping = mt_mapping.
  ENDMETHOD.

  METHOD if_qlast_visitor~ignore.
    mask = mv_ignore.
  ENDMETHOD.

  METHOD collect_entity.
    DATA(lr_entity) = REF #( mt_entities[ entity_type = iv_type
                                          entity_name = iv_name ] OPTIONAL ).
    IF lr_entity IS INITIAL.
      INSERT VALUE #( entity_name = iv_name
                      entity_type = iv_type
                      join_count  = iv_joins
                      table_count = iv_tables
                      occurrences = 1 )
             INTO TABLE mt_entities REFERENCE INTO lr_entity.
      result = abap_true.
    ELSE.
      lr_entity->occurrences = lr_entity->occurrences + 1.
    ENDIF.
  ENDMETHOD.

  METHOD collect_used_cds_view.
    DATA(lr_used_entity) = REF #( mt_used_cds_views[ name = iv_cds_view ] OPTIONAL ).
    IF lr_used_entity IS INITIAL.
      INSERT VALUE #( name  = iv_cds_view
                      count = 1 ) INTO TABLE mt_used_cds_views.
    ELSE.
      lr_used_entity->count = lr_used_entity->count + 1.
    ENDIF.
  ENDMETHOD.

  METHOD process_used_cds_view.
    ms_metrics-cds_view_count = ms_metrics-cds_view_count + 1.
    IF collect_entity( iv_type = zif_sat_c_entity_type=>cds_view
                       iv_name = CONV #( iv_ddlname ) ).
      INSERT iv_ddlname INTO TABLE mt_new_cds_views.
    ENDIF.

    collect_used_cds_view( iv_ddlname ).
  ENDMETHOD.

  METHOD process_used_view.
    DATA(lv_ddl_name) = get_ddls_name( iv_view ).
    IF lv_ddl_name IS NOT INITIAL.
      process_used_cds_view( lv_ddl_name ).
      RETURN.
    ENDIF.

    " was view already collected?
    DATA(lr_collected_view) = REF #( mt_entities[ entity_type = zif_sat_c_entity_type=>view
                                                  entity_name = iv_view ] OPTIONAL ).
    IF lr_collected_view IS BOUND.
      ms_metrics-view_count  = ms_metrics-view_count + 1.
      ms_metrics-table_count = ms_metrics-table_count + lr_collected_view->table_count.
      ms_metrics-join_count  = ms_metrics-join_count + lr_collected_view->join_count.
      RETURN.
    ENDIF.

    " Determine view type and collect view + update metrics
    IF is_external_view( iv_view_name = iv_view ).
      ms_metrics-view_count = ms_metrics-view_count + 1.
      collect_entity( iv_type = zif_sat_c_entity_type=>view
                      iv_name = iv_view ).
      RETURN.
    ENDIF.

    DATA(lt_base_tables) = get_base_tables( iv_view ).
    IF lt_base_tables IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_tables) = lines( lt_base_tables ).
    DATA(lv_joins) = 0.
    LOOP AT lt_base_tables INTO DATA(ls_base_table).
      collect_entity( iv_type = zif_sat_c_entity_type=>table
                      iv_name = ls_base_table-tabname ).
      IF sy-tabix <> 1.
        lv_joins = lv_joins + 1.
      ENDIF.
    ENDLOOP.

    ms_metrics-view_count  = ms_metrics-view_count + 1.
    ms_metrics-table_count = ms_metrics-table_count + lv_tables.
    ms_metrics-join_count  = ms_metrics-join_count + lv_joins.
    collect_entity( iv_type   = zif_sat_c_entity_type=>view
                    iv_name   = iv_view
                    iv_joins  = lv_joins
                    iv_tables = lv_tables ).
  ENDMETHOD.

  METHOD get_base_tables.
    DATA view_name TYPE viewname.

    view_name = sql_view_name.

    CALL FUNCTION 'DD_VIEW_GET'
      EXPORTING  view_name      = view_name
      TABLES     dd26v_tab_a    = result
      EXCEPTIONS access_failure = 1
                 OTHERS         = 2.
    IF ( sy-subrc <> 0 ).
      CLEAR result.
    ENDIF.
  ENDMETHOD.

  METHOD get_ddls_name.
    IF ( mo_dd_ddl_handler IS INITIAL ).
      mo_dd_ddl_handler = cl_dd_ddl_handler_factory=>create( ).
    ENDIF.

    TRY.
        result = mo_dd_ddl_handler->get_ddl_name_4_dd_artefact( ddname = iv_view_name ).
      CATCH cx_dd_ddl_exception.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.

  METHOD is_external_view.
    IF ( mo_dd_view_util IS INITIAL ).
      mo_dd_view_util = NEW cl_dd_view_utilities( ).
    ENDIF.

    TRY.
        result = mo_dd_view_util->is_external_view( iv_view_name ).
      CATCH cx_dd_ex_view.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD fill_additional_information.
    DATA lr_table_info TYPE REF TO zsat_i_databasetable.
    DATA lr_view_info TYPE REF TO zsat_i_databaseview.
    DATA lr_cds_view_info TYPE REF TO ty_cds_view_info.

    DATA(lt_cds_infos) = get_cds_infos( ).
    DATA(lt_tab_infos) = get_table_infos( ).
    DATA(lt_view_infos) = get_view_infos( ).

    LOOP AT mt_entities REFERENCE INTO DATA(lr_entity).
      IF mf_adt = abap_true.
        DATA(ls_obj_ref) = zcl_sat_adt_util=>create_adt_uri( iv_type  = lr_entity->entity_type
                                                             iv_name  = lr_entity->entity_name
                                                             iv_name2 = lr_entity->entity_name  ).
        IF ls_obj_ref-uri IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA(ls_dependency) = VALUE ty_dependency(
          adt_type      = ls_obj_ref-type
          object_type   = lr_entity->entity_type
          uri           = ls_obj_ref-uri
          metrics       = lr_entity->metrics
          used_entities = lr_entity->cds_view_count + lr_entity->view_count + lr_entity->table_count + lr_entity->table_func_count ).

      CASE lr_entity->entity_type.
        WHEN zif_sat_c_entity_type=>cds_view.
          lr_cds_view_info = REF #( lt_cds_infos[ ddlname = lr_entity->entity_name ] OPTIONAL ).
          IF lr_cds_view_info IS INITIAL.
            CONTINUE.
          ENDIF.

          ls_dependency-name        = lr_cds_view_info->entityid.
          ls_dependency-raw_name    = lr_cds_view_info->rawentityid.
          ls_dependency-api_state   = lr_cds_view_info->apistate.
          ls_dependency-source_type = lr_cds_view_info->sourcetype.
          ls_dependency-description = lr_cds_view_info->description.
          ls_dependency-package     = lr_cds_view_info->developmentpackage.

        WHEN zif_sat_c_entity_type=>view.
          lr_view_info = REF #( lt_view_infos[ viewname = lr_entity->entity_name ] OPTIONAL ).
          IF lr_view_info IS INITIAL.
            CONTINUE.
          ENDIF.

          ls_dependency-name        = lr_view_info->viewname.
          ls_dependency-raw_name    = lr_view_info->viewname.
          ls_dependency-description = lr_view_info->description.
          ls_dependency-package     = lr_view_info->developmentpackage.

        WHEN zif_sat_c_entity_type=>table.
          lr_table_info = REF #( lt_tab_infos[ tablename = lr_entity->entity_name ] OPTIONAL ).
          IF lr_table_info IS INITIAL.
            CONTINUE.
          ENDIF.

          ls_dependency-name        = lr_table_info->tablename.
          ls_dependency-raw_name    = lr_table_info->tablename.
          ls_dependency-description = lr_table_info->description.
          ls_dependency-package     = lr_table_info->developmentpackage.

      ENDCASE.

      result = VALUE #( BASE result
                        ( ls_dependency ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_ddlname_for_entity.
    " Determine DDL Name for Entity Name
    SELECT SINGLE ddlname
      FROM ddldependency
      WHERE objectname = @iv_entity_name
        AND objecttype = 'STOB'
        AND state = 'A'
      INTO @result.
  ENDMETHOD.

  METHOD get_cds_infos.
    DATA lt_ddlname_range TYPE RANGE OF ddlname.

    lt_ddlname_range = VALUE #( FOR cds IN mt_entities
                                WHERE ( entity_type = zif_sat_c_entity_type=>cds_view )
                                ( sign = 'I' option = 'EQ' low = cds-entity_name ) ).
    IF lt_ddlname_range IS INITIAL.
      RETURN.
    ENDIF.

    SELECT entityid,
           rawentityid,
           ddlname,
           sourcetype,
           \_apistate-apistate AS apistate,
           description,
           developmentpackage,
           createdby
      FROM zsat_i_cdsentity
      WHERE ddlname IN @lt_ddlname_range
      ORDER BY ddlname,
               \_apistate-apistate DESCENDING
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

  METHOD get_table_infos.
    DATA lt_table_range TYPE RANGE OF tabname.

    lt_table_range = VALUE #( FOR table IN mt_entities
                              WHERE ( entity_type = zif_sat_c_entity_type=>table )
                              ( sign = 'I' option = 'EQ' low = table-entity_name ) ).
    IF lt_table_range IS INITIAL.
      RETURN.
    ENDIF.

    SELECT tablename,
           description,
           developmentpackage,
           createdby
      FROM zsat_i_databasetable
      WHERE tablename IN @lt_table_range
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

  METHOD get_view_infos.
    DATA lt_view_range TYPE RANGE OF viewname.

    lt_view_range = VALUE #( FOR view IN mt_entities
                             WHERE ( entity_type = zif_sat_c_entity_type=>view )
                             ( sign = 'I' option = 'EQ' low = view-entity_name ) ).
    IF lt_view_range IS INITIAL.
      RETURN.
    ENDIF.

    SELECT viewname,
           description,
           developmentpackage,
           createdby
      FROM zsat_i_databaseview
      WHERE viewname IN @lt_view_range
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.
ENDCLASS.
