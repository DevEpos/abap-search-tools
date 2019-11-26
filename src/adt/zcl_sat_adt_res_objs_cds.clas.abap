"! <p class="shorttext synchronized" lang="en">Object Search Resource for CDS Search</p>
CLASS zcl_sat_adt_res_objs_cds DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_adt_res_object_search
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS post_process_result_entry
        REDEFINITION.
    METHODS post_process_result
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_objs_cds IMPLEMENTATION.

  METHOD post_process_result_entry.
    CHECK mt_ddls_source IS NOT INITIAL.

    set_ddl_positional_uri(
      EXPORTING is_result_entity = is_result_entity
      CHANGING  cs_result        = cs_result
    ).
  ENDMETHOD.

  METHOD post_process_result.
    DATA: lt_ddlname TYPE RANGE OF ddlname.

*.. Positional URI is no longer needed for where used starting with NW 7.54
    CHECK sy-saprl < 754.

*.. Read sources of all found DDLS search results to get row/column where the name of
*.... the entity is starting
    read_ddl_sources( ).
  ENDMETHOD.

ENDCLASS.
