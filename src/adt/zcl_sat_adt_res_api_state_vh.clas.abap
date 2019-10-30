"! <p class="shorttext synchronized" lang="en">Resource for reading API States</p>
CLASS zcl_sat_adt_res_api_state_vh DEFINITION
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



CLASS zcl_sat_adt_res_api_state_vh IMPLEMENTATION.
  METHOD get_named_items.
    data(lt_release_states) = cl_ris_adt_res_release_states=>get_all( i_with_longtext = abap_true ).
    p_named_item_list-items = value #(
      for release_state in lt_release_states
      ( name        = release_state-name
        description = release_state-description
        data        = release_state-longtext )
    ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.

ENDCLASS.
