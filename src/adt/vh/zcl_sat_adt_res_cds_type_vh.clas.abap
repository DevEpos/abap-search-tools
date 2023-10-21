"! <p class="shorttext synchronized">Resource for reading CDS source types</p>
CLASS zcl_sat_adt_res_cds_type_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_type_vh IMPLEMENTATION.
  METHOD get_named_items.
    p_named_item_list-items = VALUE #(
        ( name = zif_sat_c_os_cds_options=>c_type_option_value-view     description = |{ 'Standard CDS View'(001) }| )
        ( name = zif_sat_c_os_cds_options=>c_type_option_value-function description = |{ 'CDS Table Function View'(002) }| )
        ( name = zif_sat_c_os_cds_options=>c_type_option_value-extend   description = |{ 'Extension View'(003) }| ) ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
