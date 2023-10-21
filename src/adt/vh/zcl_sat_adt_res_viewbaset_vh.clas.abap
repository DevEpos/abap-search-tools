"! <p class="shorttext synchronized">Resource for base tables of ddic views</p>
CLASS zcl_sat_adt_res_viewbaset_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_viewbaset_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_tab_range TYPE RANGE OF tabname.
    DATA lt_viewclass_range TYPE RANGE OF dd25l-viewclass.

    lt_viewclass_range = VALUE #( sign   = 'I'
                                  option = 'EQ'
                                  ( low = zif_sat_c_os_view_options=>c_view_class-int-database )
                                  ( low = zif_sat_c_os_view_options=>c_view_class-int-help )
                                  ( low = zif_sat_c_os_view_options=>c_view_class-int-maintenance )
                                  ( low = zif_sat_c_os_view_options=>c_view_class-int-projection ) ).

    IF p_filter_name IS NOT INITIAL.
      lt_tab_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT DISTINCT
           basetab~tabname AS name
      FROM dd26s AS basetab
        INNER JOIN dd25l AS view
          ON basetab~viewname = view~viewname
        INNER JOIN tadir AS repo
          ON  repo~object = @zif_sat_c_tadir_types=>view
          AND repo~obj_name = view~viewname
          AND repo~genflag = ''
      WHERE basetab~tabname IN @lt_tab_range
        AND view~viewclass IN @lt_viewclass_range
      ORDER BY basetab~tabname
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
