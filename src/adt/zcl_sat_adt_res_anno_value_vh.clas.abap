"! <p class="shorttext synchronized" lang="en">Resource for reading Annotation values</p>
CLASS zcl_sat_adt_res_anno_value_vh DEFINITION
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



CLASS zcl_sat_adt_res_anno_value_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA: lt_anno_ddlname_range TYPE RANGE OF ddlaname,
          lt_anno_name_range    TYPE RANGE OF zsat_i_cdsannotationvalue-annotationnameupper,
          lt_anno_value_range   TYPE RANGE OF zsat_i_cdsannotationvalue-value.

    lt_anno_ddlname_range = VALUE #(
      ( sign = 'I' option = 'EQ' low = 'ABAPANNOTATION' )
      ( sign = 'I' option = 'EQ' low = 'COMPATIBILITYCONTRACT' )
      ( sign = 'I' option = 'CP' low = 'DEMO*' )
      ( sign = 'I' option = 'EQ' low = 'FEATURE' )
      ( sign = 'I' option = 'CP' low = 'KTD*' )
      ( sign = 'I' option = 'EQ' low = 'LANGUAGEDEPENDENCY' )
      ( sign = 'I' option = 'EQ' low = 'MAPPINGROLE' )
    ).

    IF p_filter_name IS NOT INITIAL.
      lt_anno_value_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( replace( val = p_filter_name sub = '#' with = '##' ) ) ) ).
    ENDIF.

    IF p_filter_data IS NOT INITIAL.
      lt_anno_name_range =  VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_data ) ) ).
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
