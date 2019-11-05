*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_field_visitor IMPLEMENTATION.
  METHOD visit_calc.
    CHECK mv_current_field IS NOT INITIAL.

    mf_calc_field_found = abap_true.
  ENDMETHOD.

  METHOD visit_atomic_expression.
    CHECK: object IS BOUND,
           mv_current_field IS NOT INITIAL",
*           mf_calc_field_found = abap_true.
           .

    DATA(lo_atomic_expr) = CAST cl_qlast_atomic_expression( object ).
    DATA(lv_identifier) = lo_atomic_expr->get_identifier( ).
    DATA(lo_datasource) = lo_atomic_expr->get_datasource( ).
    IF lo_datasource IS BOUND.
      DATA(lv_datasource) = lo_datasource->get_name( ).
      IF lv_identifier = mv_source_field AND lv_datasource = mv_source_entityname.
        INSERT CONV #( mv_current_field ) INTO TABLE mt_found_fields.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    mv_source_entityname = iv_source_entityname.
    mv_source_field = iv_source_field.

    m_mapping = VALUE #(
       ( classname = 'CL_QLAST_ATOMIC_EXPRESSION'      method = 'VISIT_ATOMIC_EXPRESSION'        )
*       ( classname = 'CL_QLAST_ARITHM_EXP'             method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_BOOLEAN_EXPRESSION'     method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_CASE_EXP_BASE'          method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_FUNC_EXPRESSION'        method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_FUNC_WTYPE_EXPRESSION'  method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_FUNC_NP_EXPRESSION'     method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_IS_NULL_EXPRESSION'     method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_LIKE_ESC_EXPRESSION'    method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_LIKE_EXPRESSION'        method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_IS_INITIAL_EXPRESSION'  method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_NOT_EXPRESSION'         method = 'VISIT_CALC'  )
*       ( classname = 'CL_QLAST_STDFUNC_EXPRESSION'     method = 'VISIT_CALC'  )
    ).

  ENDMETHOD.


  METHOD get_found_fields.
    rt_found_fields = mt_found_fields.
  ENDMETHOD.

ENDCLASS.
