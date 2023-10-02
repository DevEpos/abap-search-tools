"! <p class="shorttext synchronized">Resource for 'extby' parameter in CDS View search</p>
CLASS zcl_sat_adt_res_cds_ext_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_ext_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_extend_range TYPE RANGE OF ddddlsrc-parentname.

    IF p_filter_name IS NOT INITIAL.
      lt_extend_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT
      FROM zsat_i_cdsentity
      FIELDS rawentityid AS name,
             description AS description
      WHERE ddlname IN @lt_extend_range
        AND sourcetype = @zif_sat_c_cds_view_type=>extend
      ORDER BY ddlname
    INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
