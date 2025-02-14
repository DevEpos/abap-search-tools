"! <p class="shorttext synchronized">Resource for reading type of Class</p>
CLASS zcl_sat_adt_res_class_type_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_class_type_vh IMPLEMENTATION.
  METHOD get_named_items.
    p_named_item_list-items = VALUE #(
        ( name = zif_sat_c_os_clif_options=>c_class_types-class      description = |{ 'Class'(001) }| )
        ( name = zif_sat_c_os_clif_options=>c_class_types-interface  description = |{ 'Interface'(002) }| ) ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
