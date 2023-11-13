"! <p class="shorttext synchronized">CDS Parameter VH Provider</p>
CLASS zcl_sat_adt_res_cds_param_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_param_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_param_range TYPE RANGE OF dd10b-parametername.

    IF p_filter_name IS NOT INITIAL.
      lt_param_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT
      FROM dd10b AS param
      FIELDS DISTINCT param~parametername_raw AS name
      WHERE param~parametername IN @lt_param_range
      ORDER BY param~parametername_raw
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
