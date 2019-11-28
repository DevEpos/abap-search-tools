CLASS zcl_sat_adt_res_db_entity_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: get_named_items REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_db_entity_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA: lt_entity_range TYPE RANGE OF zsat_entity_id.

    IF p_filter_name IS NOT INITIAL.
      lt_entity_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT entityraw AS name,
           type AS data,
           description
      FROM zsat_i_databaseentity( p_language = @sy-langu ) AS field
      WHERE entity IN @lt_entity_range
      ORDER BY Entity
    INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.

ENDCLASS.
