"! <p class="shorttext synchronized">Resource for root table in a ddic view</p>
CLASS zcl_sat_adt_res_viewroot_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_viewroot_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_tab_range TYPE RANGE OF tabname.
    DATA lt_viewclass_range TYPE RANGE OF dd25l-viewclass.

    lt_viewclass_range = VALUE #( sign   = 'I'
                                  option = 'EQ'
                                  ( low = zif_sat_c_object_search=>c_view_class-int-database )
                                  ( low = zif_sat_c_object_search=>c_view_class-int-help )
                                  ( low = zif_sat_c_object_search=>c_view_class-int-maintenance )
                                  ( low = zif_sat_c_object_search=>c_view_class-int-projection ) ).

    IF p_filter_name IS NOT INITIAL.
      lt_tab_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT DISTINCT
           roottab AS name,
           text~ddtext AS description
      FROM dd25l AS root
        INNER JOIN tadir AS repo
          ON repo~object = @zif_sat_c_tadir_types=>view
          AND repo~obj_name = root~viewname
          AND repo~genflag = ''
        LEFT OUTER JOIN dd02t AS text
          ON root~roottab = text~tabname
          AND text~ddlanguage = @sy-langu
          AND text~as4local = 'A'
      WHERE root~roottab IN @lt_tab_range
        AND root~viewclass IN @lt_viewclass_range
      ORDER BY root~roottab
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
