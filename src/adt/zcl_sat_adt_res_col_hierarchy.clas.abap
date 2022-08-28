"! <p class="shorttext synchronized" lang="en">Resource for getting Field Hierarchy of Column</p>
CLASS zcl_sat_adt_res_col_hierarchy DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_column_info
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: internal_get REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_col_hierarchy IMPLEMENTATION.

  METHOD internal_get.
    DATA(lo_hierarchy_resolver) = NEW zcl_sat_cds_field_hier_res( ).
    ms_field_info = lo_hierarchy_resolver->resolve_field_hierarchy(
        iv_cds_view       = mv_object_name
        iv_cds_view_field = mv_field
    ).
  ENDMETHOD.

ENDCLASS.
