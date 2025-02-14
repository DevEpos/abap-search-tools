"! <p class="shorttext synchronized">Resource for value help of Structure Field</p>
CLASS zcl_sat_adt_res_structfield_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_structfield_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_field_range TYPE RANGE OF fieldname.

    IF p_filter_name IS NOT INITIAL.
      lt_field_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT FROM zsat_i_structurefieldvh AS field
      FIELDS DISTINCT fieldname AS name
      WHERE fieldname       IN @lt_field_range
        AND fieldname NOT LIKE '.%'
      ORDER BY fieldname
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
