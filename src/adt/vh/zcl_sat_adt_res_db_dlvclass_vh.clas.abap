"! <p class="shorttext synchronized">Resource for reading Class/Interface Flags (e.g. ABSTRACT)</p>
CLASS zcl_sat_adt_res_db_dlvclass_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_db_dlvclass_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA(lo_deliv_class_type_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE contflag( ) ) ).
    DATA(lt_deliv_classes) = lo_deliv_class_type_descr->get_ddic_fixed_values( ).

    p_named_item_list-items = VALUE #( FOR delivclass IN lt_deliv_classes
                                       ( name = delivclass-low description = delivclass-ddtext ) ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
