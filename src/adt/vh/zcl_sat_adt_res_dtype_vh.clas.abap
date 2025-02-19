"! <p class="shorttext synchronized">Resource for reading built-in data types</p>
CLASS zcl_sat_adt_res_dtype_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_dtype_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA(lo_datatype_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE datatype_d( ) ) ).
    p_named_item_list-items            = VALUE #( FOR <ls_fixval> IN lo_datatype_descr->get_ddic_fixed_values( )
                                                  ( name = <ls_fixval>-low description = <ls_fixval>-ddtext ) ).
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
