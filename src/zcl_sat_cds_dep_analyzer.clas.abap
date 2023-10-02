"! <p class="shorttext synchronized">Dependency Analyzer for CDS View</p>
CLASS zcl_sat_cds_dep_analyzer DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_parent,
        name TYPE string,
      END OF ty_s_parent.
    TYPES ty_t_parent TYPE STANDARD TABLE OF ty_s_parent WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_dependency,
        name                TYPE zsat_entity_id,
        raw_name            TYPE zsat_entity_id_raw,
        object_type         TYPE zsat_entity_type,
        adt_type            TYPE string,
        uri                 TYPE string,
        package             TYPE string,
        description         TYPE ddtext,
        parent_nodes        TYPE ty_t_parent,
        source_type         TYPE char1,
        api_state           TYPE string,
        occurrence          TYPE i,
        used_entities_count TYPE i,
        used_join_count     TYPE i,
        used_union_count    TYPE i,
      END OF ty_s_dependency.

    TYPES ty_t_dependency TYPE STANDARD TABLE OF ty_s_dependency WITH KEY name object_type.

    TYPES:
      BEGIN OF ty_s_dependency_info,
        cds_view          TYPE zsat_cds_view_name,
        used_entity_count TYPE i,
        dependencies      TYPE ty_t_dependency,
      END OF ty_s_dependency_info.

    TYPES:
      BEGIN OF ty_s_dependency_graph_node,
        "! db name, e.g. SQL view name
        name                     TYPE string,
        type                     TYPE string,
        relation                 TYPE string,
        entity_name              TYPE string,
        "! Entity name in camel case
        user_defined_entity_name TYPE string,
        activation_state         TYPE string,
        ddls_name                TYPE string,
        children                 TYPE REF TO data,
      END OF ty_s_dependency_graph_node,
      ty_t_dependency_graph_nodes TYPE STANDARD TABLE OF ty_s_dependency_graph_node WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_node_type,
        table                 TYPE string VALUE 'TABLE',
        cds_view              TYPE string VALUE 'CDS_VIEW',
        cds_db_view           TYPE string VALUE 'CDS_DB_VIEW',
        view                  TYPE string VALUE 'VIEW',
        cds_table_function    TYPE string VALUE 'CDS_TABLE_FUNCTION',
        external_view         TYPE string VALUE 'EXTERNAL_VIEW',
        select                TYPE string VALUE 'SELECT',
        union                 TYPE string VALUE 'UNION',
        union_all             TYPE string VALUE 'UNION_ALL',
        result                TYPE string VALUE 'RESULT',
        unknown               TYPE string VALUE 'UNKNOWN',
        related_objects_tree  TYPE string VALUE 'RELATED_OBJECTS_TREE',
        related_objects_entry TYPE string VALUE 'RELATED_OBJECTS_ENTRY',
        dcl_objects_list      TYPE string VALUE 'DCLS_OBJECT_LIST',
      END OF c_node_type,
      BEGIN OF c_relation_type,
        from             TYPE string VALUE 'FROM',
        inner_join       TYPE string VALUE 'INNER_JOIN',
        left_outer_join  TYPE string VALUE 'LEFT_OUTER_JOIN',
        right_outer_join TYPE string VALUE 'RIGHT_OUTER_JOIN',
        select           TYPE string VALUE 'SELECT',
        union            TYPE string VALUE 'UNION',
        union_all        TYPE string VALUE 'UNION_ALL',
      END OF c_relation_type,
      BEGIN OF c_activation_state,
        active       TYPE string VALUE 'ACTIVE',
        inactive     TYPE string VALUE 'INACTIVE',
        inconsistent TYPE string VALUE 'INCONSISTENT',
      END OF c_activation_state.

    "! <p class="shorttext synchronized">Anaylyzes dependencies of View and returns tree</p>
    CLASS-METHODS analyze_dependency
      IMPORTING
        iv_cds_view_name           TYPE zsat_cds_view_name
      RETURNING
        VALUE(rs_dependency_graph) TYPE ty_s_dependency_graph_node.

    "! <p class="shorttext synchronized">Retrieve distinct entities and count</p>
    CLASS-METHODS get_used_entities
      IMPORTING
        iv_cds_view_name          TYPE zsat_cds_view_name
        if_for_adt                TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_dependency_info) TYPE ty_s_dependency_info.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES BEGIN OF ty_s_dep_graph_node_ext.
            INCLUDE TYPE ty_s_dependency_graph_node.
    TYPES   parent TYPE string.
    TYPES END OF ty_s_dep_graph_node_ext.
    TYPES ty_t_dep_graph_node_ext TYPE STANDARD TABLE OF ty_s_dep_graph_node_ext WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Retrieve metrics about dependencies</p>
    CLASS-METHODS get_used_entity_count
      IMPORTING
        it_children          TYPE cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes
      CHANGING
        cv_used_entity_count TYPE i OPTIONAL
        cv_used_joins        TYPE i OPTIONAL
        cv_used_unions       TYPE i OPTIONAL.

    "! <p class="shorttext synchronized">Fill additional information of dependencies</p>
    CLASS-METHODS fill_additional_information
      IMPORTING
        if_for_adt      TYPE abap_bool OPTIONAL
      CHANGING
        ct_dependencies TYPE zcl_sat_cds_dep_analyzer=>ty_t_dependency.
ENDCLASS.


CLASS zcl_sat_cds_dep_analyzer IMPLEMENTATION.
  METHOD analyze_dependency.
    DATA(lr_dependency_visitor) = NEW cl_ddls_dependency_visitor( ).
    lr_dependency_visitor->compute_dependency_information( to_upper( iv_cds_view_name ) ).
    rs_dependency_graph = lr_dependency_visitor->get_dependency_graph( ).
  ENDMETHOD.

  METHOD get_used_entities.
    DATA lv_entity_type TYPE zsat_entity_type.
    DATA lv_entity_id TYPE zsat_entity_id.

    DATA(lo_dependency_visitor) = NEW cl_ddls_dependency_visitor( ).
    lo_dependency_visitor->compute_dependency_information( to_upper( iv_cds_view_name ) ).

    DATA(lt_dependencies) = VALUE ty_t_dep_graph_node_ext(
                                      ( CORRESPONDING #( DEEP lo_dependency_visitor->get_dependency_graph( ) ) ) ).

    LOOP AT lt_dependencies ASSIGNING FIELD-SYMBOL(<ls_dependency>).
      DATA(lv_used_entity_count) = 0.
      DATA(lv_used_union_count) = 0.
      DATA(lv_used_join_count) = 0.

      DATA(lv_tabix) = sy-tabix.
      IF <ls_dependency>-children IS BOUND.
        DATA(lt_children) = CAST cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes( <ls_dependency>-children )->*.
        get_used_entity_count( EXPORTING it_children          = lt_children
                               CHANGING  cv_used_entity_count = lv_used_entity_count
                                         cv_used_joins        = lv_used_join_count
                                         cv_used_unions       = lv_used_union_count ).
        LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<ls_child>).
          APPEND INITIAL LINE TO lt_dependencies ASSIGNING FIELD-SYMBOL(<ls_child_enhanced>).
          <ls_child_enhanced> = CORRESPONDING #( DEEP <ls_child> ).
          IF    <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-select
             OR <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-result
             OR <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-union
             OR <ls_dependency>-name = cl_ddls_dependency_visitor=>co_node_type-union_all.
            <ls_child_enhanced>-parent = <ls_dependency>-parent.
          ELSE.
            <ls_child_enhanced>-parent = COND #(
              WHEN <ls_dependency>-entity_name IS NOT INITIAL
              THEN <ls_dependency>-entity_name
              ELSE <ls_dependency>-name ).
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_tabix = 1.
        " ...... This is the starting CDS view
        rs_dependency_info-cds_view          = <ls_dependency>-entity_name.
        rs_dependency_info-used_entity_count = lv_used_entity_count.
        INSERT VALUE #( name                = to_upper( iv_cds_view_name )
                        object_type         = zif_sat_c_entity_type=>cds_view
                        occurrence          = 1
                        used_entities_count = lv_used_entity_count
                        used_join_count     = lv_used_join_count
                        used_union_count    = lv_used_union_count ) INTO TABLE rs_dependency_info-dependencies.
      ELSE.
        " ...... Add the entity to the dependency list
        IF    <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-cds_view
           OR <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-cds_table_function
           OR <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-cds_db_view.
          lv_entity_type = zif_sat_c_entity_type=>cds_view.
        ELSEIF <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-table.
          lv_entity_type = zif_sat_c_entity_type=>table.
        ELSEIF <ls_dependency>-type = cl_ddls_dependency_visitor=>co_node_type-view.
          lv_entity_type = zif_sat_c_entity_type=>view.
        ELSE.
          CLEAR lv_entity_type.
        ENDIF.

        IF lv_entity_type IS NOT INITIAL.
          lv_entity_id = COND #( WHEN <ls_dependency>-entity_name IS NOT INITIAL
                                 THEN <ls_dependency>-entity_name
                                 ELSE <ls_dependency>-name ).
          ASSIGN rs_dependency_info-dependencies[ name = lv_entity_id object_type = lv_entity_type ] TO FIELD-SYMBOL(<ls_dep>).
          IF sy-subrc <> 0.
            INSERT VALUE #( name                = lv_entity_id
                            object_type         = lv_entity_type
                            occurrence          = 1
                            used_entities_count = lv_used_entity_count
                            used_join_count     = lv_used_join_count
                            used_union_count    = lv_used_union_count
                            parent_nodes        = VALUE #( ( name = <ls_dependency>-parent ) ) ) INTO TABLE rs_dependency_info-dependencies.
          ELSE.
            <ls_dep>-occurrence   = <ls_dep>-occurrence + 1.
            <ls_dep>-parent_nodes = VALUE #( BASE <ls_dep>-parent_nodes ( name = <ls_dependency>-parent ) ).
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    fill_additional_information( EXPORTING if_for_adt      = if_for_adt
                                 CHANGING  ct_dependencies = rs_dependency_info-dependencies ).
  ENDMETHOD.

  METHOD get_used_entity_count.
    LOOP AT it_children ASSIGNING FIELD-SYMBOL(<ls_child>).
      " .... Add the entity to the dependency list
      IF    <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-cds_view
         OR <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-cds_table_function
         OR <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-cds_db_view.
        cv_used_entity_count = cv_used_entity_count + 1.
      ELSEIF <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-table.
        cv_used_entity_count = cv_used_entity_count + 1.
      ELSEIF <ls_child>-type = cl_ddls_dependency_visitor=>co_node_type-view.
        cv_used_entity_count = cv_used_entity_count + 1.
      ENDIF.

      IF    <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-inner_join
         OR <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-right_outer_join
         OR <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-left_outer_join.
        cv_used_joins = cv_used_joins + 1.
      ELSEIF    <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-union
             OR <ls_child>-relation = cl_ddls_dependency_visitor=>co_relation_type-union_all.
        cv_used_unions = cv_used_unions + 1.
      ENDIF.

      IF <ls_child>-children IS BOUND.
        get_used_entity_count(
          EXPORTING
            it_children          = CAST cl_ddls_dependency_visitor=>ty_t_dependency_graph_nodes(
                                     <ls_child>-children )->*
          CHANGING
            cv_used_entity_count = cv_used_entity_count
            cv_used_joins        = cv_used_joins
            cv_used_unions       = cv_used_unions ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_additional_information.
    DATA lt_entity_range TYPE RANGE OF zsat_entity_id.
    DATA lt_tabname_range TYPE RANGE OF tabname.

    lt_entity_range = VALUE #( FOR cds IN ct_dependencies
                               WHERE ( object_type = zif_sat_c_entity_type=>cds_view )
                               ( sign = 'I' option = 'EQ' low = cds-name ) ).
    lt_tabname_range = VALUE #( FOR table IN ct_dependencies
                                WHERE ( object_type <> zif_sat_c_entity_type=>cds_view )
                                ( sign = 'I' option = 'EQ' low = table-name ) ).

    " .. Enrich dependencies by description
    IF lt_entity_range IS NOT INITIAL.
      IF if_for_adt = abap_true.
        SELECT entityid AS entity,
               rawentityid AS entityraw,
               ddlname AS secondary_entity_id,
               sourcetype AS source_type,
               \_apistate-apistate AS api_state,
               description,
               developmentpackage,
               createdby
          FROM zsat_i_cdsentity
          WHERE ddlname IN @lt_entity_range
             OR entityid IN @lt_entity_range
        INTO TABLE @DATA(lt_entity_info).
      ELSE.
        SELECT entityid AS entity,
               rawentityid AS entityraw,
               cdsbase~ddlname AS secondary_entity_id,
               description,
               developmentpackage,
               createdby
          FROM zsat_p_cdsviewbase AS cdsbase
            LEFT OUTER JOIN zsat_i_cdsviewt AS text
              ON cdsbase~ddlname = text~ddlname
              AND text~language = @sy-langu
          WHERE cdsbase~ddlname IN @lt_entity_range
             OR cdsbase~entityid IN @lt_entity_range
        INTO CORRESPONDING FIELDS OF TABLE @lt_entity_info.
      ENDIF.
    ENDIF.

    IF lt_tabname_range IS NOT INITIAL.
      SELECT entity,
             entityraw,
             description,
             developmentpackage,
             createdby
        FROM zsat_i_databasetablesandviews
        WHERE entity IN @lt_tabname_range
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_entity_info.
    ENDIF.

    LOOP AT ct_dependencies ASSIGNING FIELD-SYMBOL(<ls_cds_dep>).
      SORT <ls_cds_dep>-parent_nodes BY name.
      DELETE ADJACENT DUPLICATES FROM <ls_cds_dep>-parent_nodes COMPARING name.
      ASSIGN lt_entity_info[ entity = <ls_cds_dep>-name ] TO FIELD-SYMBOL(<ls_entity_descr>).
      IF sy-subrc <> 0.
        ASSIGN lt_entity_info[ secondary_entity_id = <ls_cds_dep>-name ] TO <ls_entity_descr>.
      ENDIF.
      CHECK sy-subrc = 0.
      <ls_cds_dep>-name        = <ls_entity_descr>-entity.
      <ls_cds_dep>-raw_name    = <ls_entity_descr>-entityraw.
      <ls_cds_dep>-api_state   = <ls_entity_descr>-api_state.
      <ls_cds_dep>-source_type = <ls_entity_descr>-source_type.
      <ls_cds_dep>-description = <ls_entity_descr>-description.
      <ls_cds_dep>-package     = <ls_entity_descr>-developmentpackage.
      IF if_for_adt = abap_true.
        DATA(ls_obj_ref) = zcl_sat_adt_util=>create_adt_uri(
                               iv_type  = <ls_cds_dep>-object_type
                               iv_name  = <ls_entity_descr>-entity
                               iv_name2 = CONV #( <ls_entity_descr>-secondary_entity_id ) ).
        <ls_cds_dep>-adt_type = ls_obj_ref-type.
        <ls_cds_dep>-uri      = ls_obj_ref-uri.
      ENDIF.
    ENDLOOP.

    SORT ct_dependencies BY used_entities_count DESCENDING
                            occurrence DESCENDING.
  ENDMETHOD.
ENDCLASS.
