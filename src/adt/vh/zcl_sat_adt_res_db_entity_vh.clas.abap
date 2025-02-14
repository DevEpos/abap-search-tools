CLASS zcl_sat_adt_res_db_entity_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_db_entity_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_entity_range TYPE RANGE OF zsat_entity_id.

    IF p_filter_name IS NOT INITIAL.
      lt_entity_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT FROM zsat_i_databaseentity AS field
      FIELDS entityraw   AS name,
             type        AS data,
             description
      WHERE entity IN @lt_entity_range
      ORDER BY entity
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    LOOP AT p_named_item_list-items REFERENCE INTO DATA(lr_db_entity).
      lr_db_entity->data = |imageId=| &&
        SWITCH #( lr_db_entity->data
                  WHEN zif_sat_c_entity_type=>cds_view THEN zif_sat_c_object_types=>structured_object
                  WHEN zif_sat_c_entity_type=>table    THEN zif_sat_c_object_types=>table_definition
                  WHEN zif_sat_c_entity_type=>view     THEN zif_sat_c_object_types=>view_definition ).
    ENDLOOP.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
