"! <p class="shorttext synchronized" lang="en">Resource for getting the table types</p>
CLASS zcl_sat_adt_res_table_type_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_named_items
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_table_type_vh IMPLEMENTATION.
  METHOD get_named_items.
    p_named_item_list-items = VALUE #(
      ( name = zif_sat_c_object_search=>c_type_option_value-table description = |{ 'Transparent Table'(001) }| )
      ( name = zif_sat_c_object_search=>c_type_option_value-view  description = |{ 'Database View'(002) }| )

    ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.

ENDCLASS.
