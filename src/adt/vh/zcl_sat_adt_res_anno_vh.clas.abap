"! <p class="shorttext synchronized" lang="en">Annotation value for Search Parameter 'anno'</p>
CLASS zcl_sat_adt_res_anno_vh DEFINITION
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



CLASS zcl_sat_adt_res_anno_vh IMPLEMENTATION.

  METHOD get_named_items.
    DATA: lt_anno_ddlname_range TYPE RANGE OF ddlaname,
          lt_key_name_range     TYPE RANGE OF ddla_rt_header-key_raw.

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
      lt_key_name_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT
      FROM ddla_rt_header
      FIELDS key_raw AS name,
             'annotation' AS data
      WHERE ddlaname NOT IN @lt_anno_ddlname_range
        AND key_upper IN @lt_key_name_range
      ORDER BY key_raw
    INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.

ENDCLASS.
