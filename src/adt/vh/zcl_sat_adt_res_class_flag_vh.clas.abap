"! <p class="shorttext synchronized">Resource for reading Class/Interface Flags (e.g. ABSTRACT)</p>
CLASS zcl_sat_adt_res_class_flag_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_class_flag_vh IMPLEMENTATION.
  METHOD get_named_items.
    p_named_item_list-items = VALUE #(
        ( name = zif_sat_c_os_clif_options=>c_class_intf_flags-is_abstract         description = |{ 'Abstract Class'(001) }| )
        ( name = zif_sat_c_os_clif_options=>c_class_intf_flags-is_final            description = |{ 'Final Class'(002) }| )
        ( name = zif_sat_c_os_clif_options=>c_class_intf_flags-has_test            description = |{ 'Class with ABAP Unit Test'(003) }| )
        ( name        = zif_sat_c_os_clif_options=>c_class_intf_flags-is_fixpoint
          description = |{ 'Class with Fixed Point Arithmetic'(004) }| )
        ( name        = zif_sat_c_os_clif_options=>c_class_intf_flags-is_shared_memory
          description = |{ 'Class is Shared Memory Enabled'(005) }| )
        ( name = zif_sat_c_os_clif_options=>c_class_intf_flags-has_unicode_checks  description = |{ 'Unicode Checks are Active'(006) }| ) ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
