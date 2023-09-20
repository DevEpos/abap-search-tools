"! <p class="shorttext synchronized">Filters methods by sub components</p>
CLASS zcl_sat_meth_subco_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_result              TYPE REF TO zif_sat_ty_object_search=>ty_t_search_result
        if_use_and_for_options TYPE abap_bool OPTIONAL
        it_meth_param_filter   TYPE zif_sat_ty_object_search=>ty_class_subcomp_range
        it_meth_exc_filter     TYPE zif_sat_ty_object_search=>ty_class_subcomp_range.

    METHODS apply.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_comp,
        clsname TYPE seocompo-clsname,
        cmpname TYPE seocompo-cmpname,
      END OF ty_comp,

      BEGIN OF ty_sub_comp,
        clsname TYPE seosubco-clsname,
        cmpname TYPE seosubco-cmpname,
        sconame TYPE seosubco-sconame,
        scotype TYPE seosubco-scotype,
      END OF ty_sub_comp.

    DATA mr_method_result TYPE REF TO zif_sat_ty_object_search=>ty_t_search_result.
    DATA mt_meth_param_filter TYPE zif_sat_ty_object_search=>ty_class_subcomp_range.
    DATA mt_meth_exc_filter TYPE zif_sat_ty_object_search=>ty_class_subcomp_range.

    DATA mt_sub_components TYPE SORTED TABLE OF ty_sub_comp WITH NON-UNIQUE KEY clsname cmpname.
    DATA mt_matching_comp TYPE SORTED TABLE OF ty_comp WITH UNIQUE KEY clsname cmpname.
    DATA mf_use_and_for_options TYPE abap_bool.

    METHODS select_sub_components.

    METHODS find_subcomp_matches.
    METHODS apply_excluding_filters.

    METHODS conv_excl_filter_to_incl
      IMPORTING
        it_filter     TYPE zif_sat_ty_object_search=>ty_class_subcomp_range
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_class_subcomp_range.
ENDCLASS.


CLASS zcl_sat_meth_subco_filter IMPLEMENTATION.
  METHOD constructor.
    mt_meth_param_filter = it_meth_param_filter.
    mt_meth_exc_filter = it_meth_exc_filter.
    mr_method_result = ir_result.
    mf_use_and_for_options = if_use_and_for_options.
  ENDMETHOD.

  METHOD apply.
    CHECK mr_method_result->* IS NOT INITIAL.

    IF     mt_meth_param_filter IS INITIAL
       AND mt_meth_exc_filter   IS INITIAL.
      RETURN.
    ENDIF.

    select_sub_components( ).

    IF mt_sub_components IS INITIAL.
      CLEAR mr_method_result->*.
      RETURN.
    ENDIF.

    " delete methods where a component was excluded via filtering
    apply_excluding_filters( ).

    " remove excluding filters
    DELETE mt_meth_param_filter WHERE sign = 'E'.
    DELETE mt_meth_exc_filter WHERE sign = 'E'.

    IF     mt_meth_param_filter IS INITIAL
       AND mt_meth_exc_filter   IS INITIAL.
      RETURN.
    ENDIF.

    find_subcomp_matches( ).

    " delete all results that do not match the found sub components
    LOOP AT mr_method_result->* REFERENCE INTO DATA(lr_result).
      IF NOT line_exists( mt_matching_comp[ clsname = lr_result->method_decl_clif
                                            cmpname = lr_result->method_decl_method ] ).
        DELETE mr_method_result->*.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD select_sub_components.
    DATA lt_dyn_select TYPE string_table.
    DATA lt_comp_type_filter TYPE RANGE OF seoscotype.

    DATA(lt_comp_filter) = VALUE zif_sat_ty_object_search=>ty_class_subcomp_range( ( LINES OF mt_meth_param_filter )
                                                                                   ( LINES OF mt_meth_exc_filter ) ).

    " We consider all filters as I(ncluding) during the select
    LOOP AT lt_comp_filter REFERENCE INTO DATA(lr_comp_filter).
      lr_comp_filter->sign = 'I'.
    ENDLOOP.

    IF mt_meth_param_filter IS INITIAL.
      lt_comp_type_filter = VALUE #( ( sign = 'I' option = 'EQ' low = seos_scotype_exception ) ).
    ENDIF.

    IF mt_meth_exc_filter IS INITIAL.
      lt_comp_type_filter = VALUE #( ( sign = 'I' option = 'EQ' low = seos_scotype_parameter ) ).
    ENDIF.

    lt_dyn_select = VALUE #( ( `clsname` ) ( `,cmpname` ) ).

    IF line_exists( lt_comp_filter[ sign = 'I' ] ).
      lt_dyn_select = VALUE #( BASE lt_dyn_select ( `,sconame` ) ( `,scotype` ) ).
    ENDIF.

    " select params/exceptions for methods by filter
    SELECT DISTINCT (lt_dyn_select)
      FROM seosubco
      FOR ALL ENTRIES IN @mr_method_result->*
      WHERE clsname = @mr_method_result->*-method_decl_clif
        AND cmpname = @mr_method_result->*-method_decl_method
        AND scotype IN @lt_comp_type_filter
        AND sconame IN @lt_comp_filter
      INTO CORRESPONDING FIELDS OF TABLE @mt_sub_components.
  ENDMETHOD.

  METHOD find_subcomp_matches.
    DATA lr_sub_comp TYPE REF TO zcl_sat_meth_subco_filter=>ty_sub_comp.

    DATA(lv_exc_filter_count) = lines( mt_meth_exc_filter ).
    DATA(lv_param_filter_count) = lines( mt_meth_param_filter ).
    DATA(lv_subcomp_filter_count) = lv_exc_filter_count + lv_param_filter_count.

    " Group sub components that match both param and exception filters
    " TODO: consider 'AND' flag of search options
    LOOP AT mt_sub_components REFERENCE INTO lr_sub_comp
      GROUP BY ( classname = lr_sub_comp->clsname
                 methodname = lr_sub_comp->cmpname
                 size = GROUP SIZE ) REFERENCE INTO DATA(lr_group).

      IF mf_use_and_for_options = abap_true AND lr_group->size < lv_subcomp_filter_count.
        CONTINUE.
      ENDIF.

      DATA(lv_exc_match_count) = 0.
      DATA(lv_param_match_count) = 0.

      LOOP AT GROUP lr_group REFERENCE INTO DATA(lr_group_entry).
        IF lr_group_entry->scotype = seos_scotype_parameter.
          IF lr_group_entry->sconame IN mt_meth_param_filter.
            lv_param_match_count = lv_param_match_count + 1.
          ENDIF.
        ELSEIF lr_group_entry->scotype = seos_scotype_exception.
          IF lr_group_entry->sconame IN mt_meth_exc_filter.
            lv_exc_match_count = lv_exc_match_count + 1.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA(lf_method_match) = abap_false.

      IF mf_use_and_for_options = abap_true.
        IF     lv_exc_match_count   >= lv_exc_filter_count
           AND lv_param_match_count >= lv_param_filter_count.
          lf_method_match = abap_true.
        ENDIF.
      ELSEIF lv_exc_filter_count > 0 AND lv_param_filter_count > 0.
        lf_method_match = xsdbool( lv_exc_match_count >= 1 AND lv_param_match_count >= 1 ).
      ELSEIF lv_exc_filter_count > 0.
        lf_method_match = xsdbool( lv_exc_match_count >= 1 ).
      ELSEIF lv_param_filter_count > 0.
        lf_method_match = xsdbool( lv_param_match_count >= 1 ).
      ENDIF.

      IF lf_method_match = abap_true.
        INSERT VALUE #( clsname = lr_group->classname
                        cmpname = lr_group->methodname ) INTO TABLE mt_matching_comp.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD apply_excluding_filters.
    DATA(lt_exc_filters) = conv_excl_filter_to_incl( mt_meth_exc_filter ).
    DATA(lt_param_filters) = conv_excl_filter_to_incl( mt_meth_param_filter ).

    IF lt_exc_filters IS INITIAL AND lt_param_filters IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT mt_sub_components REFERENCE INTO DATA(lr_sub_comp).
      IF    (     lr_sub_comp->scotype  = seos_scotype_exception
              AND lt_exc_filters       IS NOT INITIAL
              AND lr_sub_comp->sconame IN lt_exc_filters )
         OR (     lr_sub_comp->scotype  = seos_scotype_parameter
              AND lt_param_filters     IS NOT INITIAL
              AND lr_sub_comp->sconame IN lt_param_filters ).
        DELETE mr_method_result->* WHERE     method_decl_clif   = lr_sub_comp->clsname
                                         AND method_decl_method = lr_sub_comp->cmpname.
        DELETE mt_sub_components WHERE     clsname = lr_sub_comp->clsname
                                       AND cmpname = lr_sub_comp->cmpname.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD conv_excl_filter_to_incl.
    LOOP AT it_filter INTO DATA(ls_filter) WHERE sign = 'E'.
      ls_filter-sign = 'I'.
      result = VALUE #( BASE result ( ls_filter ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
