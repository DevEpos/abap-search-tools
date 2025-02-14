"! <p class="shorttext synchronized">Dependency Analyzer for CDS View</p>
CLASS zcl_sat_cds_dep_analyzer DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
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

    CONSTANTS: BEGIN OF c_node_type,
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
               END OF c_node_type.
    CONSTANTS: BEGIN OF c_relation_type,
                 from             TYPE string VALUE 'FROM',
                 inner_join       TYPE string VALUE 'INNER_JOIN',
                 left_outer_join  TYPE string VALUE 'LEFT_OUTER_JOIN',
                 right_outer_join TYPE string VALUE 'RIGHT_OUTER_JOIN',
                 select           TYPE string VALUE 'SELECT',
                 union            TYPE string VALUE 'UNION',
                 union_all        TYPE string VALUE 'UNION_ALL',
               END OF c_relation_type.
    CONSTANTS: BEGIN OF c_activation_state,
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

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_cds_dep_analyzer IMPLEMENTATION.
  METHOD analyze_dependency.
    DATA(lo_dependency_visitor) = NEW cl_ddls_dependency_visitor( ).
    lo_dependency_visitor->compute_dependency_information( to_upper( iv_cds_view_name ) ).
    rs_dependency_graph = lo_dependency_visitor->get_dependency_graph( ).
  ENDMETHOD.
ENDCLASS.
