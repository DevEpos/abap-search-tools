"! <p class="shorttext synchronized">API Contracts</p>
CLASS zcl_sat_api_state_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_result) TYPE REF TO zcl_sat_api_state_util.

    METHODS is_object_type_supported
      IMPORTING
        i_object_type   TYPE string
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS get_filter_values_for_otype
      IMPORTING
        i_object_type   TYPE string
      RETURNING
        VALUE(r_result) TYPE zif_sat_ty_adt_types=>ty_r_api_adt_filter_values.

  PRIVATE SECTION.
    TYPES ty_contract_id TYPE c LENGTH 2.

    CONSTANTS c_api_view_name TYPE ddlname VALUE 'ARS_ADT_API_FILTER'.
    CONSTANTS c_compatibility_contract_fld TYPE string VALUE 'COMPATIBILITY_CONTRACT' ##NO_TEXT.
    CONSTANTS c_release_state_fld TYPE string VALUE 'RELEASE_STATE' ##NO_TEXT.
    CONSTANTS c_filter_value_alias TYPE string VALUE 'FILTER_VALUE' ##NO_TEXT.
    CONSTANTS c_released TYPE string VALUE 'RELEASED'.
    CONSTANTS c_deprecated TYPE string VALUE 'DEPRECATED'.

    CONSTANTS c_c0 TYPE ty_contract_id VALUE 'C0'.
    CONSTANTS c_c1 TYPE ty_contract_id VALUE 'C1'.
    CONSTANTS c_c2 TYPE ty_contract_id VALUE 'C2'.

    TYPES:
      ty_ddl_sources TYPE STANDARD TABLE OF ddddlsrc WITH EMPTY KEY,
      BEGIN OF ty_contract_mapping,
        mapped_value  TYPE string,
        contract_id   TYPE ty_contract_id,
        release_state TYPE string,
      END OF ty_contract_mapping,

      ty_contract_mappings TYPE STANDARD TABLE OF ty_contract_mapping WITH EMPTY KEY,

      BEGIN OF ty_contract_obj_info,
        object_type           TYPE trobjtype,
        c0_supported          TYPE abap_bool,
        c1_supported          TYPE abap_bool,
        c2_supported          TYPE abap_bool,
        deprecation_supported TYPE abap_bool,
      END OF ty_contract_obj_info.

    CLASS-DATA instance TYPE REF TO zcl_sat_api_state_util.

    DATA mapped_contracts TYPE ty_contract_mappings.

    METHODS constructor.
    METHODS get_api_contract_mapping.

    METHODS collect_mapping
      IMPORTING
        i_select TYPE REF TO cl_qlast_select.

    METHODS get_filter_val_field
      IMPORTING
        i_select        TYPE REF TO cl_qlast_select
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS get_compat_contract
      IMPORTING
        i_select        TYPE REF TO cl_qlast_select
      EXPORTING
        e_contract_id   TYPE ty_contract_id
        e_release_state TYPE string.

    METHODS get_field_name
      IMPORTING
        i_expr          TYPE REF TO cl_qlast_simple_expression
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS get_equals_value
      IMPORTING
        i_expr          TYPE REF TO cl_qlast_expression
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS read_api_view
      RETURNING
        VALUE(r_result) TYPE ty_ddl_sources.

    METHODS get_object_type_info
      IMPORTING
        i_object_type   TYPE string
      RETURNING
        VALUE(r_result) TYPE ty_contract_obj_info.

    METHODS remove_invalid_contracts
      IMPORTING
        i_object_info TYPE ty_contract_obj_info
      CHANGING
        x_contracts   TYPE ty_contract_mappings.
ENDCLASS.


CLASS zcl_sat_api_state_util IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.

    r_result = instance.
  ENDMETHOD.

  METHOD constructor.
    get_api_contract_mapping( ).
  ENDMETHOD.

  METHOD is_object_type_supported.
    r_result = xsdbool( get_object_type_info( i_object_type )-object_type IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_filter_values_for_otype.
    DATA(object_info) = get_object_type_info( i_object_type ).

    DATA(contracts) = mapped_contracts.

    remove_invalid_contracts( EXPORTING i_object_info = object_info
                              CHANGING  x_contracts   = contracts ).

    r_result = VALUE #( FOR contract IN contracts ( sign = 'I' option = 'EQ' low = contract-mapped_value ) ).
    r_result = VALUE #( BASE r_result ( sign = 'I' option = 'EQ' low = c_released ) ).

    IF object_info-deprecation_supported = abap_true.
      r_result = VALUE #( BASE r_result ( sign = 'I' option = 'EQ' low = c_deprecated ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_api_contract_mapping.
    DATA(ddl_sources) = read_api_view( ).
    DATA(parser) = NEW cl_ddl_parser( ).

    DATA(stmnt) = CAST cl_qlast_view_definition( parser->parse_cds( it_sources = ddl_sources
                                                                    iv_bitset  = cl_ddl_parser=>set_bitmask(
                                                                                     iv_semantic      = abap_true
                                                                                     iv_ars_check_off = abap_true
                                                                                     iv_trace         = abap_false
                                                                                     iv_locally       = abap_false
                                                                                     iv_aiepp         = abap_false
                                                                                     iv_extresol      = abap_true ) ) ).

    DATA(select) = stmnt->get_select( ).

    WHILE select IS BOUND.
      collect_mapping( select ).
      select = select->get_union( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD get_object_type_info.
    SPLIT i_object_type AT '/' INTO DATA(object_type) DATA(sub_obj_type).

    IF sy-saprl < '754'.
      SELECT SINGLE tadir_type AS object_type,
             c0_supported,
             c1_supported,
             c2_supported
        FROM ars_adt_cntrsup
        WHERE objtype_tr = @object_type
          AND subtype_wb = @sub_obj_type
        INTO CORRESPONDING FIELDS OF @r_result.
    ELSE.
      SELECT SINGLE *
        FROM ars_s_obj_type
        WHERE object_type = @object_type
          AND workbench_object_type = @sub_obj_type
        INTO CORRESPONDING FIELDS OF @r_result.
    ENDIF.
  ENDMETHOD.

  METHOD remove_invalid_contracts.
    DATA contract_id_filter TYPE RANGE OF ty_contract_id.

    IF i_object_info-c0_supported = abap_false.
      contract_id_filter = VALUE #( BASE contract_id_filter ( sign = 'I' option = 'EQ' low = c_c0 ) ).
    ENDIF.
    IF i_object_info-c1_supported = abap_false.
      contract_id_filter = VALUE #( BASE contract_id_filter ( sign = 'I' option = 'EQ' low = c_c1 ) ).
    ENDIF.
    IF i_object_info-c2_supported = abap_false.
      contract_id_filter = VALUE #( BASE contract_id_filter ( sign = 'I' option = 'EQ' low = c_c2 ) ).
    ENDIF.

    IF contract_id_filter IS NOT INITIAL.
      DELETE x_contracts WHERE contract_id IN contract_id_filter.
    ENDIF.

    IF i_object_info-deprecation_supported = abap_false.
      DELETE x_contracts WHERE release_state <> c_released.
    ENDIF.

    " Empty contract id means custom field registry entry, which is only
    " relevant for BDEF, DDLS, FUNC
    IF i_object_info-object_type <> zif_sat_c_tadir_types=>data_definition.
      DELETE x_contracts WHERE contract_id IS INITIAL.
    ENDIF.
  ENDMETHOD.

  METHOD collect_mapping.
    DATA contract TYPE ty_contract_mapping.

    contract-mapped_value = get_filter_val_field( i_select ).
    get_compat_contract( EXPORTING i_select        = i_select
                         IMPORTING e_contract_id   = contract-contract_id
                                   e_release_state = contract-release_state ).

    mapped_contracts = VALUE #( BASE mapped_contracts ( contract ) ).
  ENDMETHOD.

  METHOD get_filter_val_field.
    LOOP AT i_select->get_selectlist( )->get_entries( ) INTO DATA(select_entry).
      IF select_entry->get_type( ) <> cl_qlast_constants=>selectlist_entry_std.
        CONTINUE.
      ENDIF.

      DATA(std_entry) = CAST cl_qlast_stdselectlist_entry( select_entry ).

      IF std_entry->get_alias( ) <> c_filter_value_alias.
        CONTINUE.
      ENDIF.

      IF std_entry->get_expression( )->get_type( ) <> cl_qlast_constants=>expressiontype_cast_function.
        CONTINUE.
      ENDIF.

      DATA(cast_expr) = CAST cl_qlast_cast_expression( std_entry->get_expression( ) ).
      DATA(inner_expr) = cast_expr->get_expression( ).
      IF inner_expr->get_type( ) <> cl_qlast_constants=>expressiontype_literal.
        CONTINUE.
      ENDIF.

      DATA(literal_expr) = CAST cl_qlast_literal_expression( inner_expr ).
      r_result = literal_expr->get_unescaped_value( ).
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_compat_contract.
    DATA(cond_expr) = i_select->get_where( ).
    IF cond_expr->get_type( ) <> cl_qlast_constants=>expressiontype_and.
      RETURN.
    ENDIF.

    DATA(and_expr) = CAST cl_qlast_boolean_expression( cond_expr ).

    LOOP AT and_expr->get_conditions( ) INTO DATA(condition).
      IF condition->get_type( ) <> cl_qlast_constants=>expressiontype_eq.
        CONTINUE.
      ENDIF.

      DATA(eq_expr) = CAST cl_qlast_comp_expression( condition ).
      DATA(field_name) = get_field_name( i_expr = eq_expr->get_left( ) ).
      IF field_name = c_compatibility_contract_fld.
        e_contract_id = get_equals_value( i_expr = eq_expr->get_right( ) ).
      ELSEIF field_name = c_release_state_fld.
        e_release_state = get_equals_value( i_expr = eq_expr->get_right( ) ).
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_field_name.
    IF i_expr->get_type( ) <> cl_qlast_constants=>expressiontype_element.
      RETURN.
    ENDIF.

    DATA(atomic_expr) = CAST cl_qlast_atomic_expression( i_expr ).
    r_result = atomic_expr->get_identifier( ).
  ENDMETHOD.

  METHOD get_equals_value.
    IF i_expr->get_type( ) = cl_qlast_constants=>expressiontype_domain_value.
      DATA(dom_val_expr) = CAST cl_qlast_domain_value_expr( i_expr ).
      r_result = replace( val = dom_val_expr->get_value( ) sub = '''' with = space occ = 0 ).
    ELSEIF i_expr->get_type( ) = cl_qlast_constants=>expressiontype_literal.
      DATA(literal_expr) = CAST cl_qlast_literal_expression( i_expr ).
      r_result = literal_expr->get_unescaped_value( ).
    ENDIF.
  ENDMETHOD.

  METHOD read_api_view.
    SELECT *
      FROM ddddlsrc
      WHERE ddlname = @c_api_view_name
        AND as4local = 'A'
      INTO TABLE @r_result.
  ENDMETHOD.
ENDCLASS.
