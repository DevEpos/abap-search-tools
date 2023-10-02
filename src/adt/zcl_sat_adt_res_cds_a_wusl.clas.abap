"! <p class="shorttext synchronized">Where-Used-Analysis for Table/View/CDS in CDS Views</p>
CLASS zcl_sat_adt_res_cds_a_wusl DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_source_origin,
        select_from TYPE string VALUE 'from',
        association TYPE string VALUE 'assoc',
      END OF c_source_origin.

    TYPES BEGIN OF ty_wusl_result.
            INCLUDE TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds.
    TYPES   source_entity TYPE tabname.
    TYPES END OF ty_wusl_result.

    TYPES:
      ty_wusl_results TYPE STANDARD TABLE OF ty_wusl_result WITH EMPTY KEY,

      BEGIN OF ty_wusl_result_ref,
        ddlname TYPE tabname,
        ref     TYPE REF TO ty_wusl_result,
      END OF ty_wusl_result_ref,

      ty_wusl_result_refs TYPE SORTED TABLE OF ty_wusl_result_ref WITH NON-UNIQUE KEY ddlname,

      BEGIN OF ty_result_key,
        ddlname TYPE tabname,
      END OF ty_result_key,

      ty_result_keys TYPE TABLE OF ty_result_key.

    DATA:
      BEGIN OF ms_sql,
        select TYPE string_table,
        from   TYPE string_table,
        where  TYPE string_table,
      END OF ms_sql.

    DATA mt_result TYPE ty_wusl_results.
    DATA mt_result_refs TYPE ty_wusl_result_refs.
    DATA mt_result_keys TYPE ty_result_keys.
    DATA mv_entity TYPE zsat_entity_id.
    DATA mv_source_origin TYPE string.
    DATA mf_recursive_search TYPE abap_bool.

    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

    METHODS execute_where_used_in_cds
      RAISING
        cx_adt_rest.

    METHODS fill_response
      IMPORTING
        io_response TYPE REF TO if_adt_rest_response
      RAISING
        cx_adt_rest.

    METHODS build_sql_for_assoc_search.
    METHODS build_sql_for_from_search.

    METHODS run_search
      RAISING
        cx_adt_rest.

    METHODS build_sql.
    METHODS fill_descriptions.
    METHODS fill_other_properties.

    METHODS find_all_resursively
      RAISING
        cx_adt_rest.

    METHODS get_cds_sql_name
      IMPORTING
        iv_entity     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS do_after_search.
ENDCLASS.


CLASS zcl_sat_adt_res_cds_a_wusl IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).
    execute_where_used_in_cds( ).
    fill_response( io_response = response ).
  ENDMETHOD.

  METHOD get_parameters.
    mv_entity = to_upper( zcl_sat_adt_res_util=>get_request_param_value(
                              io_request    = io_request
                              iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-entity_name
                              if_mandatory  = abap_true ) ).
    mv_source_origin = zcl_sat_adt_res_util=>get_request_param_value(
                           io_request    = io_request
                           iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-source_origin
                           if_mandatory  = abap_true ).
    mf_recursive_search = zcl_sat_adt_res_util=>get_boolean_req_param(
                              iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-recursive_search
                              io_request    = io_request ).
  ENDMETHOD.

  METHOD execute_where_used_in_cds.
    build_sql( ).

    IF mv_source_origin = c_source_origin-select_from.
      build_sql_for_from_search( ).
    ELSEIF mv_source_origin = c_source_origin-association.
      build_sql_for_assoc_search( ).
    ENDIF.

    run_search( ).

    IF mf_recursive_search = abap_true AND mv_source_origin = c_source_origin-select_from.
      find_all_resursively( ).
    ENDIF.

    do_after_search( ).
  ENDMETHOD.

  METHOD fill_response.
    io_response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_where_used_in_cds_res_h( )
        data            = CORRESPONDING zif_sat_ty_adt_types=>ty_where_used_in_cds_t(  mt_result ) ).
  ENDMETHOD.

  METHOD build_sql.
    ms_sql = VALUE #(
        select = VALUE #( ( `base~rawentityid as entity_name,` )
                          ( `base~sourcetype as source_type,` )
                          ( `base~ddlname` ) )

        from   = VALUE #( ( |{ get_cds_sql_name( zif_sat_c_select_source_id=>zsat_p_cds ) } as base| ) ) ).
  ENDMETHOD.

  METHOD build_sql_for_assoc_search.
    ms_sql-from  = VALUE #(
        BASE ms_sql-from
        ( |inner join { zif_sat_c_select_source_id=>dd08b } as assoc on base~ddlname = assoc~strucobjn | ) ).
    ms_sql-where = VALUE #( ( `assoc~strucobjn_t = @mv_entity ` ) ).
  ENDMETHOD.

  METHOD build_sql_for_from_search.
    ms_sql-from  = VALUE #(
        BASE ms_sql-from
        ( |inner join { get_cds_sql_name( zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity ) } as selectpart | )
        ( | on base~viewname = selectpart~ddlviewname | ) ).
    ms_sql-where = VALUE #( ( `selectpart~sourceentity = @mv_entity ` ) ).
  ENDMETHOD.

  METHOD get_cds_sql_name.
    SELECT SINGLE viewname FROM zsat_i_ddldependency WHERE entityname = @iv_entity INTO @result.
  ENDMETHOD.

  METHOD run_search.
    TRY.
        SELECT DISTINCT (ms_sql-select)
          FROM  (ms_sql-from)
          WHERE (ms_sql-where)
          ORDER BY base~rawentityid ASCENDING
          INTO CORRESPONDING FIELDS OF TABLE @mt_result.
      CATCH cx_sy_dynamic_osql_error INTO DATA(lx_sql_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_rest_error
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text( lx_sql_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD fill_descriptions.
    DATA lt_texts TYPE STANDARD TABLE OF seu_objtxt.

    CHECK mt_result_keys IS NOT INITIAL.

    lt_texts = VALUE #( FOR entity IN mt_result_keys
                        ( object = zif_sat_c_tadir_types=>data_definition obj_name = entity-ddlname ) ).

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES obj_tab = lt_texts.

    LOOP AT mt_result_refs REFERENCE INTO DATA(lr_result_ref).
      DATA(lr_text) = REF #( lt_texts[ obj_name = lr_result_ref->ddlname ] OPTIONAL ).
      IF lr_text IS BOUND.
        lr_result_ref->ref->description = lr_text->stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_other_properties.
    CHECK mt_result_keys IS NOT INITIAL.

    LOOP AT mt_result_keys REFERENCE INTO DATA(lr_result_key).
      DATA(lv_uri) = zcl_sat_adt_util=>get_adt_object_ref_uri(
                         iv_name = CONV #( lr_result_key->ddlname )
                         is_type = VALUE #( objtype_tr = zif_sat_c_tadir_types=>data_definition subtype_wb = 'DF' ) ).
      DATA(lv_type) = zif_sat_c_object_types=>data_definition.

      LOOP AT mt_result_refs REFERENCE INTO DATA(lr_result_ref) WHERE ddlname = lr_result_key->ddlname.
        lr_result_ref->ref->uri  = lv_uri.
        lr_result_ref->ref->type = lv_type.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_all_resursively.
    DATA lt_usages LIKE mt_result.
    DATA lt_current_parent_refs TYPE SORTED TABLE OF ty_wusl_result_ref WITH NON-UNIQUE KEY ddlname.
    DATA lt_tmp_parent_refs LIKE lt_current_parent_refs.
    DATA lr_usage_stored TYPE REF TO ty_wusl_result.

    FIELD-SYMBOLS <lt_wusl_children> TYPE ty_wusl_results.

    CHECK mt_result IS NOT INITIAL.

    lt_current_parent_refs = VALUE #( FOR <result> IN mt_result
                                      ( ddlname = <result>-ddlname
                                        ref     = REF #( <result> ) ) ).

    ms_sql-select = VALUE #( BASE ms_sql-select ( |,selectpart~sourceentity as source_entity| ) ).

    WHILE lt_current_parent_refs IS NOT INITIAL.
      SELECT DISTINCT (ms_sql-select)
        FROM  (ms_sql-from)
        FOR ALL ENTRIES IN @lt_current_parent_refs
        WHERE selectpart~sourceentity = @lt_current_parent_refs-ddlname
        INTO CORRESPONDING FIELDS OF TABLE @lt_usages.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      LOOP AT lt_usages REFERENCE INTO DATA(lr_usage).
        LOOP AT lt_current_parent_refs REFERENCE INTO DATA(lr_parent) WHERE ddlname = lr_usage->source_entity.
          IF lr_parent->ref->children IS INITIAL.
            lr_parent->ref->children = NEW ty_wusl_results( ).
          ENDIF.

          ASSIGN lr_parent->ref->children->* TO <lt_wusl_children>.
          APPEND lr_usage->* TO <lt_wusl_children> REFERENCE INTO lr_usage_stored.

          IF lr_usage_stored IS BOUND.
            lt_tmp_parent_refs = VALUE #( BASE lt_tmp_parent_refs ( ddlname = lr_usage->ddlname ref = lr_usage_stored ) ).
            mt_result_refs = VALUE #( BASE mt_result_refs ( ddlname = lr_usage->ddlname ref = lr_usage_stored ) ).
          ENDIF.
        ENDLOOP.

        CLEAR lr_usage_stored.
        UNASSIGN <lt_wusl_children>.
      ENDLOOP.

      lt_current_parent_refs = lt_tmp_parent_refs.
      CLEAR lt_tmp_parent_refs.

    ENDWHILE.
  ENDMETHOD.

  METHOD do_after_search.
    " Results at root level still need to be added to refs
    LOOP AT mt_result REFERENCE INTO DATA(lr_result).
      mt_result_refs = VALUE #( BASE mt_result_refs ( ddlname = lr_result->ddlname ref = lr_result ) ).
    ENDLOOP.

    mt_result_keys = CORRESPONDING #( mt_result_refs ).
    SORT mt_result_keys.
    DELETE ADJACENT DUPLICATES FROM mt_result_keys.

    fill_descriptions( ).
    fill_other_properties( ).
  ENDMETHOD.
ENDCLASS.
