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
    TYPES   filtervalue   TYPE zsat_i_apistates-filtervalue.
    TYPES   source_entity TYPE tabname.
    TYPES END OF ty_wusl_result.

    TYPES:
      ty_wusl_results TYPE STANDARD TABLE OF ty_wusl_result WITH EMPTY KEY,

      BEGIN OF ty_cds_parent_ref,
        ddlname TYPE tabname,
        ref     TYPE REF TO ty_wusl_result,
      END OF ty_cds_parent_ref.

    DATA:
      BEGIN OF ms_sql,
        select TYPE string_table,
        from   TYPE string_table,
        where  TYPE string_table,
      END OF ms_sql.

    DATA mt_result                  TYPE ty_wusl_results.
    DATA mv_entity                  TYPE zsat_entity_id.
    DATA mf_only_local_assocs       TYPE abap_bool.
    DATA mv_source_origin           TYPE string.
    DATA mf_released_entitites_only TYPE abap_bool.
    DATA mf_recursive_search        TYPE abap_bool.

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

    METHODS fill_descriptions
      CHANGING
        ct_where_used TYPE ty_wusl_results.

    METHODS fill_other_properties
      CHANGING
        ct_where_used TYPE ty_wusl_results.

    METHODS find_all_resursively
      RAISING
        cx_adt_rest.
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
    mf_released_entitites_only = zcl_sat_adt_res_util=>get_boolean_req_param(
        iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_released_entities
        io_request    = io_request ).
    mf_only_local_assocs = zcl_sat_adt_res_util=>get_boolean_req_param(
                               iv_param_name = zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_local_assocs
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
    fill_descriptions( CHANGING ct_where_used = mt_result ).
    fill_other_properties( CHANGING ct_where_used = mt_result ).

    IF mf_recursive_search = abap_true AND mv_source_origin = c_source_origin-select_from.
      find_all_resursively( ).
    ENDIF.
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
                          ( `base~ddlname,` )
                          ( `api~apistate as api_state,` )
                          ( `api~filtervalue` ) )

        from   = VALUE #(
            LET lv_join_type = COND string( WHEN mf_released_entitites_only = abap_true THEN 'inner' ELSE 'left outer' ) IN
            ( |{ zif_sat_c_select_source_id=>zsat_i_cdsentity } as base| )
            ( |{ lv_join_type } join { zif_sat_c_select_source_id=>zsat_i_apistates } as api | )
            ( ` on  api~objectname = base~ddlname ` )
            ( ` and api~objecttype = 'DDLS' ` ) ) ).
  ENDMETHOD.

  METHOD build_sql_for_assoc_search.
    ms_sql-from  = VALUE #(
        BASE ms_sql-from
        ( |inner join { zif_sat_c_select_source_id=>dd08b } as assoc on base~ddlname = assoc~strucobjn | ) ).
    ms_sql-where = VALUE #( ( `assoc~strucobjn_t = @mv_entity ` ) ).

    IF mf_only_local_assocs = abap_true.
      ms_sql-where = VALUE #( BASE ms_sql-where
                              ( `and assoc~strucobjn_s = @space ` ) ).
    ENDIF.
  ENDMETHOD.

  METHOD build_sql_for_from_search.
    ms_sql-from  = VALUE #( BASE ms_sql-from
                            ( |inner join { zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity } as selectpart | )
                            ( | on base~viewname = selectpart~ddlviewname | ) ).
    ms_sql-where = VALUE #( ( `selectpart~sourceentity = @mv_entity ` ) ).
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

    CHECK ct_where_used IS NOT INITIAL.

    lt_texts = VALUE #( FOR entity IN ct_where_used
                        ( object = zif_sat_c_tadir_types=>data_definition obj_name = entity-ddlname ) ).

    SORT lt_texts BY obj_name
                     object.
    DELETE ADJACENT DUPLICATES FROM lt_texts COMPARING obj_name object.

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES obj_tab = lt_texts.

    LOOP AT ct_where_used ASSIGNING FIELD-SYMBOL(<ls_entity>).
      ASSIGN lt_texts[ obj_name = <ls_entity>-ddlname ] TO FIELD-SYMBOL(<ls_text>).
      IF sy-subrc = 0.
        <ls_entity>-description = <ls_text>-stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_other_properties.
    CHECK ct_where_used IS NOT INITIAL.

    DATA(lt_results) = ct_where_used.
    SORT lt_results BY ddlname.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING ddlname.

    LOOP AT lt_results REFERENCE INTO DATA(lr_unique_result).
      lr_unique_result->uri  = zcl_sat_adt_util=>get_adt_object_ref_uri(
          iv_name = CONV #( lr_unique_result->ddlname )
          is_type = VALUE #( objtype_tr = zif_sat_c_tadir_types=>data_definition subtype_wb = 'DF' ) ).
      lr_unique_result->type = zif_sat_c_object_types=>data_definition.
      IF lr_unique_result->filtervalue = zif_sat_c_cds_api_state=>add_custom_fields.
        CLEAR lr_unique_result->api_state.
      ENDIF.

      LOOP AT ct_where_used REFERENCE INTO DATA(lr_result) WHERE ddlname = lr_unique_result->ddlname.
        lr_result->uri       = lr_unique_result->uri.
        lr_result->type      = lr_unique_result->type.
        lr_result->api_state = lr_unique_result->api_state.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_all_resursively.
    DATA lt_usages              LIKE mt_result.
    DATA lt_current_parent_refs TYPE SORTED TABLE OF ty_cds_parent_ref WITH NON-UNIQUE KEY ddlname.
    DATA lt_tmp_parent_refs     LIKE lt_current_parent_refs.
    DATA lr_usage_stored        TYPE REF TO ty_wusl_result.

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

      fill_descriptions( CHANGING ct_where_used = lt_usages ).
      fill_other_properties( CHANGING ct_where_used = lt_usages ).

      LOOP AT lt_usages REFERENCE INTO DATA(lr_usage).
        LOOP AT lt_current_parent_refs REFERENCE INTO DATA(lr_parent) WHERE ddlname = lr_usage->source_entity.
          IF lr_parent->ref->children IS INITIAL.
            lr_parent->ref->children = NEW ty_wusl_results( ).
          ENDIF.

          ASSIGN lr_parent->ref->children->* TO <lt_wusl_children>.
          APPEND lr_usage->* TO <lt_wusl_children> REFERENCE INTO lr_usage_stored.

          IF lr_usage_stored IS BOUND.
            lt_tmp_parent_refs = VALUE #( BASE lt_tmp_parent_refs ( ddlname = lr_usage->ddlname ref = lr_usage_stored ) ).
          ENDIF.
        ENDLOOP.

        CLEAR lr_usage_stored.
        UNASSIGN <lt_wusl_children>.
      ENDLOOP.

      lt_current_parent_refs = lt_tmp_parent_refs.
      CLEAR lt_tmp_parent_refs.

    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
