"! <p class="shorttext synchronized">Table Data Class VH</p>
CLASS zcl_sat_adt_res_db_datacls_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_db_datacls_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA(lv_langu) = sy-langu.
    IF lv_langu <> 'E' AND lv_langu <> 'D'.
      lv_langu = 'E'.
    ENDIF.

    SELECT text~tabart   AS name,
           text~darttext AS description
      FROM ddart AS data_class
           INNER JOIN dartt AS text
             ON  data_class~tabart = text~tabart
             AND text~ddlangu      = @lv_langu
      WHERE data_class~ddclass <> 'INT' " hide internal classes
      ORDER BY data_class~tabart
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items.

    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
