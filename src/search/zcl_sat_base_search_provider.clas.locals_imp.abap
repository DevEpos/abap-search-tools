*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_api_state_filter_helper IMPLEMENTATION.
  METHOD constructor.
    mo_search_provider = io_search_provider.
    mt_tadir_type = it_tadir_type.
    mt_values = it_values.
    mv_ref_field = iv_ref_field.
    mv_ref_table_alias = iv_ref_table_alias.
    mv_api_state_subquery = |SELECT DISTINCT { c_fields-object_name } | && cl_abap_char_utilities=>cr_lf &&
                            | FROM { zif_sat_c_select_source_id=>zsat_i_apistates } | && cl_abap_char_utilities=>cr_lf &&
                            | WHERE |.
  ENDMETHOD.

  METHOD create_api_filter.
    separate_filters( ).

    IF has_including_filters( ).
      build_join( ).
      add_filters( ).
    ENDIF.

    IF mo_search_provider->ms_search_engine_params-use_and_cond_for_options = abap_true.
      add_having_clauses( ).
    ELSEIF has_including_filters( ).
      mo_search_provider->set_distinct_required( ).
    ENDIF.

    add_excluding_filters( ).
  ENDMETHOD.

  METHOD separate_filters.
    DATA lt_state_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.
    DATA lt_value_filters TYPE zif_sat_ty_object_search=>ty_t_value_range.

    LOOP AT mt_values INTO DATA(ls_value).
      CASE ls_value-low.
        WHEN zif_sat_c_object_search=>c_api_option_value-released OR
             zif_sat_c_object_search=>c_api_option_value-deprecated.

          lt_state_filters = VALUE #( BASE lt_state_filters
                                      ( ls_value ) ).
        WHEN OTHERS.
          lt_value_filters = VALUE #( BASE lt_value_filters
                                      ( ls_value ) ).
      ENDCASE.
    ENDLOOP.

    mo_search_provider->split_including_excluding( EXPORTING it_values    = lt_state_filters
                                                   IMPORTING et_including = mt_i_state_filters
                                                             et_excluding = mt_e_state_filters ).

    mo_search_provider->split_including_excluding( EXPORTING it_values    = lt_value_filters
                                                   IMPORTING et_including = mt_i_value_filters
                                                             et_excluding = mt_e_value_filters ).
  ENDMETHOD.

  METHOD build_join.
    mo_search_provider->add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_apistates }|
                                        iv_alias      = c_api_alias
                                        it_conditions = build_join_conditions( ) ).
  ENDMETHOD.

  METHOD add_filters.
    IF mt_i_state_filters IS NOT INITIAL AND mt_i_value_filters IS NOT INITIAL.
      mo_search_provider->new_or_cond_list( ).
      IF mt_i_value_filters IS NOT INITIAL.
        mo_search_provider->add_option_filter( iv_fieldname = |{ c_api_alias }~{ c_fields-filter_value }|
                                               it_values    = mt_i_value_filters ).
      ENDIF.
      mo_search_provider->new_or_cond_list( ).
      IF mt_i_state_filters IS NOT INITIAL.
        mo_search_provider->add_option_filter( iv_fieldname = |{ c_api_alias }~{ c_fields-api_state }|
                                               it_values    = mt_i_state_filters ).
      ENDIF.
    ELSEIF mt_i_state_filters IS NOT INITIAL.
      mo_search_provider->add_option_filter( iv_fieldname = |{ c_api_alias }~{ c_fields-api_state }|
                                             it_values    = mt_i_state_filters ).
    ELSE.
      mo_search_provider->add_option_filter( iv_fieldname = |{ c_api_alias }~{ c_fields-filter_value }|
                                             it_values    = mt_i_value_filters ).
    ENDIF.
  ENDMETHOD.

  METHOD add_having_clauses.
    IF lines( mt_i_value_filters ) > 1.
      mo_search_provider->add_having_clause( iv_field           = |{ c_api_alias }~{ c_fields-filter_value }|
                                             iv_counter_compare = lines( mt_i_value_filters ) ).
    ENDIF.

    IF lines( mt_i_state_filters ) > 1.
      mo_search_provider->add_having_clause( iv_field           = |{ c_api_alias }~{ c_fields-api_state }|
                                             iv_counter_compare = lines( mt_i_state_filters ) ).
    ENDIF.
  ENDMETHOD.

  METHOD build_join_conditions.
    result = VALUE #( ( field           = c_fields-object_name
                        tabname_alias   = CONV #( c_api_alias )
                        ref_field       = mv_ref_field
                        ref_table_alias = mv_ref_table_alias
                        type            = zif_sat_c_join_cond_type=>field ) ).
    LOOP AT mt_tadir_type INTO DATA(lv_type).
      result = VALUE #( BASE result
                        ( field         = c_fields-object_type
                          tabname_alias = CONV #( c_api_alias )
                          value         = lv_type
                          type          = zif_sat_c_join_cond_type=>filter
                          and_or        = COND #( WHEN sy-tabix = 1
                                                  THEN zif_sat_c_selection_condition=>and
                                                  ELSE zif_sat_c_selection_condition=>or ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_excluding_filters.
    CHECK    mt_e_state_filters IS NOT INITIAL
          OR mt_e_value_filters IS NOT INITIAL.

    " objectType = ... AND ( apistate = ... OR filtervalue = ... )
    DATA(lt_excl_for_where) = zcl_sat_where_clause_builder=>create_and_condition(
        VALUE #(
            ( VALUE #(
                  ( values = VALUE #( FOR type IN mt_tadir_type
                                      ( sign = 'I' option = 'EQ' low = type sqlfieldname = c_fields-object_type ) ) ) ) )
            ( VALUE #( ( LINES OF get_subquery_or_seltab( iv_fieldname = c_fields-api_state
                                                          it_values    = mt_e_state_filters ) )
                       ( LINES OF
                         get_subquery_or_seltab( iv_fieldname = c_fields-filter_value
                                                 it_values    = mt_e_value_filters ) ) ) ) ) ).

    DATA(lv_excluding_subquery) = mv_api_state_subquery.
    LOOP AT lt_excl_for_where INTO DATA(lv_excl_where).
      lv_excluding_subquery = |{ lv_excluding_subquery }{ cl_abap_char_utilities=>cr_lf }{ lv_excl_where }|.
    ENDLOOP.

    mo_search_provider->add_subquery_filter( iv_fieldname = |{ mv_ref_table_alias }~{ mv_ref_field }|
                                             iv_subquery  = lv_excluding_subquery
                                             iv_option    = zif_sat_c_options=>not_in_subquery ).
  ENDMETHOD.

  METHOD get_subquery_or_seltab.
    CHECK it_values IS NOT INITIAL.

    result = VALUE #( ( values = VALUE zif_sat_ty_global=>ty_t_seltab_sql(
                                           ( LINES OF VALUE #( FOR value IN it_values
                                                               LET opt  = value-option
                                                                   sign = zif_sat_c_options=>including IN
                                                               ( sqlfieldname = iv_fieldname
                                                                 sign         = sign
                                                                 option       = opt
                                                                 low          = value-low ) ) ) ) ) ).
  ENDMETHOD.

  METHOD has_including_filters.
    result = xsdbool( mt_i_state_filters IS NOT INITIAL OR mt_i_value_filters IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.
