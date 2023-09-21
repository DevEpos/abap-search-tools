"! <p class="shorttext synchronized">Resource for reading Annotation values</p>
CLASS zcl_sat_adt_res_anno_value_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
    TYPES ty_t_anno_name_range TYPE RANGE OF string.
    TYPES ty_t_anno_value_range TYPE RANGE OF string.
ENDCLASS.


CLASS zcl_sat_adt_res_anno_value_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_anno_value_range TYPE ty_t_anno_value_range.
    DATA lt_anno_name_range TYPE ty_t_anno_name_range.

    IF p_filter_name IS NOT INITIAL.
      lt_anno_value_range = VALUE #( ( sign   = 'I'
                                       option = 'CP'
                                       low    = to_upper( replace( val = p_filter_name sub = '#' with = '##' ) ) ) ).
    ENDIF.

    IF p_filter_data IS NOT INITIAL.
      lt_anno_name_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_data ) ) ).
    ENDIF.

    SELECT
      FROM zsat_i_cdsannotationvalue
      FIELDS DISTINCT
             value AS name
      WHERE annotationnameupper IN @lt_anno_name_range
        AND upper( value )      IN @lt_anno_value_range
      ORDER BY value
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
