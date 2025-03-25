CLASS zcl_sat_adt_res_orig_syst_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_orig_syst_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_original_system_range TYPE RANGE OF df14l-ps_posid.

    IF p_filter_name IS NOT INITIAL.
      lt_original_system_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT originalsystem AS name FROM zsat_i_originalsystemvh
      WHERE originalsystem IN @lt_original_system_range
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
