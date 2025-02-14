"! <p class="shorttext synchronized">Where-Used-in CDS Analysis</p>
CLASS zcl_sat_cds_wusi_analysis DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_source_origin,
        select_from TYPE string VALUE 'from',
        association TYPE string VALUE 'assoc',
      END OF c_source_origin.

    TYPES BEGIN OF ty_wusl_result.
            INCLUDE TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds.
    TYPES   source_entity         TYPE tabname.
    TYPES   has_released_children TYPE abap_bool.
    TYPES   is_released           TYPE abap_bool.
    TYPES   parent_ref            TYPE REF TO data.
    TYPES END OF ty_wusl_result.

    TYPES ty_wusl_results TYPE STANDARD TABLE OF ty_wusl_result WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        iv_entity           TYPE zsat_entity_id
        iv_source_origin    TYPE string
        if_recursive        TYPE abap_bool OPTIONAL
        if_only_local_assoc TYPE abap_bool OPTIONAL
        if_only_released    TYPE abap_bool OPTIONAL.

    METHODS run
      RAISING
        zcx_sat_application_exc.

    METHODS get_result
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_where_used_in_cds_t.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_wusl_result_ref,
             ddlname TYPE tabname,
             ref     TYPE REF TO ty_wusl_result,
           END OF ty_wusl_result_ref,

           ty_wusl_result_refs TYPE SORTED TABLE OF ty_wusl_result_ref WITH NON-UNIQUE KEY ddlname.

    TYPES: BEGIN OF ty_result_key,
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
    DATA mf_only_local_assocs TYPE abap_bool.
    DATA mv_source_origin TYPE string.
    DATA mf_released_entitites_only TYPE abap_bool.
    DATA mf_api_state_join_active TYPE abap_bool.
    DATA mf_recursive_search TYPE abap_bool.

    METHODS build_sql_for_assoc_search.
    METHODS build_sql_for_from_search.

    METHODS find_references
      RAISING
        zcx_sat_application_exc.

    METHODS build_sql.
    METHODS fill_descriptions.
    METHODS fill_other_properties.
    METHODS find_all_resursively.
    METHODS add_api_state_info.

    METHODS remove_non_released_views.

    METHODS propagate_released_child_stat
      IMPORTING
        ir_wusl_result TYPE REF TO ty_wusl_result.

    METHODS remove_non_rel_recursive
      IMPORTING
        ir_wusl_results TYPE REF TO ty_wusl_results.

    METHODS collect_result_keys.
ENDCLASS.


CLASS zcl_sat_cds_wusi_analysis IMPLEMENTATION.
  METHOD constructor.
    mv_entity = iv_entity.
    mv_source_origin = iv_source_origin.
    mf_only_local_assocs = if_only_local_assoc.
    mf_recursive_search = if_recursive.
    mf_released_entitites_only = if_only_released.
  ENDMETHOD.

  METHOD run.
    build_sql( ).

    IF mv_source_origin = c_source_origin-select_from.
      build_sql_for_from_search( ).
    ELSEIF mv_source_origin = c_source_origin-association.
      build_sql_for_assoc_search( ).
    ENDIF.

    find_references( ).

    IF mf_recursive_search = abap_true AND mv_source_origin = c_source_origin-select_from.
      find_all_resursively( ).
    ENDIF.

    " Results at root level still need to be added to refs
    LOOP AT mt_result REFERENCE INTO DATA(lr_result).
      mt_result_refs = VALUE #( BASE mt_result_refs
                                ( ddlname = lr_result->ddlname ref = lr_result ) ).
    ENDLOOP.

    collect_result_keys( ).

    add_api_state_info( ).

    IF     mf_released_entitites_only = abap_true
       AND mf_api_state_join_active   = abap_false.
      remove_non_released_views( ).
    ENDIF.

    fill_descriptions( ).
    fill_other_properties( ).
  ENDMETHOD.

  METHOD get_result.
    result = CORRESPONDING #( mt_result ).
  ENDMETHOD.

  METHOD build_sql.
    ms_sql = VALUE #( select = VALUE #( ( `base~rawentityid as entity_name,` )
                                        ( `base~sourcetype as source_type,` )
                                        ( `base~ddlname` ) )
                      from   = VALUE #( ( |{ zif_sat_c_select_source_id=>zsat_p_cds } as base | ) ) ).

    IF mf_released_entitites_only = abap_true AND mf_recursive_search = abap_false.
      mf_api_state_join_active = abap_true.
      ms_sql-from = VALUE #( BASE ms_sql-from
                             ( |inner join { zif_sat_c_select_source_id=>zsat_i_apistates } as api | )
                             ( ` on  api~objectname = base~ddlname ` )
                             ( | and api~objecttype = '{ zif_sat_c_tadir_types=>data_definition }' | )
                             ( | and api~apistate   = '{ zif_sat_c_cds_api_state=>released }' | ) ).
    ENDIF.
  ENDMETHOD.

  METHOD build_sql_for_assoc_search.
    ms_sql-from  = VALUE #(
        BASE ms_sql-from
        ( |inner join { zif_sat_c_select_source_id=>dd08b } as assoc on base~ddlname = assoc~strucobjn | ) ).
    ms_sql-where = VALUE #( ( `assoc~strucobjn_t = @mv_entity ` ) ).

    IF mf_only_local_assocs = abap_true.
      ms_sql-where = VALUE #( BASE ms_sql-where
                              ( |AND case when assoc~strucobjn_s = @space or assoc~strucobjn_s = assoc~strucobjn | )
                              ( |           then @abap_true| )
                              ( |         else @space end = @abap_true | ) ).
    ENDIF.
  ENDMETHOD.

  METHOD build_sql_for_from_search.
    ms_sql-from  = VALUE #( BASE ms_sql-from
                            ( |inner join { zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity } as selectpart | )
                            ( | on base~viewname = selectpart~ddlviewname | ) ).
    ms_sql-where = VALUE #( ( `selectpart~sourceentity = @mv_entity ` ) ).
  ENDMETHOD.

  METHOD find_references.
    TRY.
        SELECT DISTINCT (ms_sql-select)
          FROM (ms_sql-from)
          WHERE (ms_sql-where)
          ORDER BY base~rawentityid ASCENDING
          INTO CORRESPONDING FIELDS OF TABLE @mt_result.
      CATCH cx_sy_dynamic_osql_error INTO DATA(lx_sql_error).
        RAISE EXCEPTION TYPE zcx_sat_application_exc
          EXPORTING previous = lx_sql_error.
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
                         is_type = VALUE #( objtype_tr = zif_sat_c_tadir_types=>data_definition
                                            subtype_wb = 'DF' ) ).
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

    ms_sql-select = VALUE #( BASE ms_sql-select
                             ( |,selectpart~sourceentity as source_entity| ) ).

    WHILE lt_current_parent_refs IS NOT INITIAL.
      SELECT DISTINCT (ms_sql-select)
        FROM (ms_sql-from)
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
          lr_usage_stored->parent_ref = lr_parent->ref.

          lt_tmp_parent_refs = VALUE #( BASE lt_tmp_parent_refs
                                        ( ddlname = lr_usage->ddlname ref = lr_usage_stored ) ).
          mt_result_refs = VALUE #( BASE mt_result_refs
                                    ( ddlname = lr_usage->ddlname ref = lr_usage_stored ) ).
        ENDLOOP.

        CLEAR lr_usage_stored.
        IF <lt_wusl_children> IS ASSIGNED.
          SORT <lt_wusl_children> BY ddlname.
        ENDIF.
        UNASSIGN <lt_wusl_children>.
      ENDLOOP.

      lt_current_parent_refs = lt_tmp_parent_refs.
      CLEAR lt_tmp_parent_refs.

    ENDWHILE.
  ENDMETHOD.

  METHOD add_api_state_info.
    CHECK mt_result_keys IS NOT INITIAL.

    IF mf_api_state_join_active = abap_false.
      SELECT DISTINCT objectname,
                      apistate
        FROM zsat_i_apistates
        FOR ALL ENTRIES IN @mt_result_keys
        WHERE objectname = @mt_result_keys-ddlname
          AND objecttype = @zif_sat_c_tadir_types=>data_definition
        INTO TABLE @DATA(lt_api_states).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT mt_result_refs REFERENCE INTO DATA(lr_result_ref).
      IF mf_api_state_join_active = abap_false.
        lr_result_ref->ref->api_state   = VALUE #( lt_api_states[ objectname = lr_result_ref->ref->ddlname ]-apistate OPTIONAL ).
        lr_result_ref->ref->is_released = xsdbool( lr_result_ref->ref->api_state CS zif_sat_c_cds_api_state=>released ).
      ELSE.
        lr_result_ref->ref->api_state = zif_sat_c_cds_api_state=>released.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD remove_non_released_views.
    LOOP AT mt_result_refs REFERENCE INTO DATA(lr_result_ref) WHERE ref->is_released = abap_true.
      IF     lr_result_ref->ref->parent_ref IS BOUND
         AND CAST ty_wusl_result( lr_result_ref->ref->parent_ref )->has_released_children  = abap_false.
        propagate_released_child_stat( CAST #( lr_result_ref->ref->parent_ref ) ).
      ENDIF.
    ENDLOOP.

    DELETE mt_result_refs WHERE     ref->is_released           = abap_false
                                AND ref->has_released_children = abap_false.
    remove_non_rel_recursive( REF #( mt_result ) ).

    " refill the result keys to skip unnecessary reads
    collect_result_keys( ).
  ENDMETHOD.

  METHOD propagate_released_child_stat.
    DATA(lr_parent) = ir_wusl_result.

    WHILE lr_parent IS BOUND.
      lr_parent->has_released_children = abap_true.
      lr_parent = CAST #( lr_parent->parent_ref ).
    ENDWHILE.
  ENDMETHOD.

  METHOD remove_non_rel_recursive.
    LOOP AT ir_wusl_results->* REFERENCE INTO DATA(lr_result).
      IF     lr_result->is_released           = abap_false
         AND lr_result->has_released_children = abap_false.
        DELETE ir_wusl_results->*.
      ELSE.
        IF lr_result->children IS BOUND.
          remove_non_rel_recursive( CAST #( lr_result->children ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD collect_result_keys.
    IF mt_result IS INITIAL.
      CLEAR: mt_result_keys,
             mt_result_refs.
      RETURN.
    ENDIF.

    mt_result_keys = CORRESPONDING #( mt_result_refs ).
    SORT mt_result_keys.
    DELETE ADJACENT DUPLICATES FROM mt_result_keys.
  ENDMETHOD.
ENDCLASS.
