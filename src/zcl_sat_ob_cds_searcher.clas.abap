"! <p class="shorttext synchronized" lang="en">CDS View Searcher for Object Browser</p>
CLASS zcl_sat_ob_cds_searcher DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_ob_generic_searcher.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_searcher.
    "! <p class="shorttext synchronized" lang="en">CLASS-CONSTRUCTOR</p>
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
    METHODS determine_grouping
        REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA gv_field_subquery TYPE string.
    CLASS-DATA gv_anno_subquery TYPE string.
    CLASS-DATA gv_select_from_subquery TYPE string.
    CLASS-DATA gv_assoc_subquery TYPE string.
    CLASS-DATA gv_param_subquery TYPE string.
    CLASS-DATA gv_params_subquery TYPE string.

    CONSTANTS:
      c_base_alias                TYPE string VALUE 'base' ##NO_TEXT,
      c_anno_alias                TYPE string VALUE 'anno' ##NO_TEXT,
      c_extension_view_alias      TYPE string VALUE 'ext' ##no_text,
      c_api_alias                 TYPE string VALUE 'api' ##NO_TEXT,
      c_param_alias               TYPE string VALUE 'param' ##NO_TEXT,
      c_select_from_alias         TYPE string VALUE 'frompart' ##NO_TEXT,
      c_parameterized_view_alias  TYPE string VALUE 'paramviews' ##NO_TEXT,
      c_used_in_association_alias TYPE string VALUE 'associationusage' ##NO_TEXT,
      BEGIN OF c_fields,
        alias    TYPE string VALUE 'field' ##NO_TEXT,
        entityid TYPE string VALUE 'entityid' ##NO_TEXT,
        name     TYPE string VALUE 'name' ##NO_TEXT,
        value    TYPE string VALUE 'value' ##NO_TEXT,
      END OF c_fields.

    DATA mv_param_filter_count TYPE i.
    DATA mv_field_filter_count TYPE i.
    DATA mv_anno_filter_count TYPE i.
    DATA mv_from_filter_count TYPE i.
    DATA mv_assoc_filter_count TYPE i.

    "! <p class="shorttext synchronized" lang="en">Create filter for TYPE option</p>
    "!
    METHODS add_type_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for ANNO option</p>
    "!
    METHODS add_anno_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for PARAM option</p>
    "!
    METHODS add_param_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FIELD option</p>
    METHODS add_field_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for ASSOC option</p>
    METHODS add_association_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FROM option</p>
    METHODS add_from_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for API option</p>
    METHODS add_api_option_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Adds extensions filter to query</p>
    METHODS add_extensions_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_browser=>ty_search_option_values-value_range.
    "! <p class="shorttext synchronized" lang="en">Add API state information for results</p>
    METHODS add_api_state_info.
    "! <p class="shorttext synchronized" lang="en">Add search terms of query to search filter</p>
    METHODS add_search_terms_to_search.
ENDCLASS.



CLASS zcl_sat_ob_cds_searcher IMPLEMENTATION.
  METHOD class_constructor.
*.. Create sub queries for parameters where boolean operation AND is senseful
    gv_field_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_cdsviewfield } | && c_cr_lf &&
                        | WHERE |.
    gv_anno_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                       | FROM { zif_sat_c_select_source_id=>zsat_i_cdsannotation } | && c_cr_lf &&
                       | WHERE |.
    gv_assoc_subquery = |SELECT DISTINCT ddlname | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_associatedincds } | && c_cr_lf &&
                        | WHERE |.
    gv_select_from_subquery = |SELECT DISTINCT ddlviewname | && c_cr_lf &&
                              | FROM { zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity } | && c_cr_lf &&
                              | WHERE |.
    gv_param_subquery = |SELECT DISTINCT parametername | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>dd10b } | && c_cr_lf &&
                        | WHERE |.
    gv_params_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                         | FROM { zif_sat_c_select_source_id=>zsat_i_cdsviewwithparameter }|.
  ENDMETHOD.

  METHOD zif_sat_object_searcher~search.

    set_base_select_table(
        iv_entity = zif_sat_c_select_source_id=>zsat_i_cdsentity
        iv_alias  = c_base_alias
    ).

    add_select_field( iv_fieldname = c_fields-entityid iv_fieldname_alias = 'entity_id' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'sourcetype' iv_fieldname_alias = 'source_type' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'ddlname'  iv_fieldname_alias = 'secondary_entity_id' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'createdby' iv_fieldname_alias = 'created_by' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'rawentityid' iv_fieldname_alias = 'entity_id_raw' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'description' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = 'developmentpackage' iv_fieldname_alias = 'devclass' iv_entity = c_base_alias ).
    add_select_field( iv_fieldname = |'C'| iv_fieldname_alias = 'entity_type' ).
    add_select_field( iv_fieldname = |'DDLS'| iv_fieldname_alias = 'tadir_type' ).

    add_order_by( iv_fieldname = c_fields-entityid iv_entity = c_base_alias  ).

    IF mo_search_query->has_search_terms( ).
      add_search_terms_to_search( ).
    ENDIF.

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      CASE <ls_option>-option.

*.......... Find views which have a certain extension
        WHEN zif_sat_c_object_browser=>c_search_option-by_extensions.
          add_extensions_filter( EXPORTING it_values = <ls_option>-value_range ).

*.......... Find views via its description
        WHEN zif_sat_c_object_browser=>c_search_option-by_description.
          add_option_filter(
            iv_fieldname = mv_description_filter_field
            it_values    = <ls_option>-value_range
          ).

*.......... Find views with a certain responsible person
        WHEN zif_sat_c_object_browser=>c_search_option-by_owner.
          add_option_filter(
            iv_fieldname = 'createdby'
            it_values    = <ls_option>-value_range
          ).

*.......... Find views which exist in a certain development package
        WHEN zif_sat_c_object_browser=>c_search_option-by_package.
          add_option_filter(
            iv_fieldname = 'developmentpackage'
            it_values    = <ls_option>-value_range
          ).

*.......... Find views regarding the release status of the cds view
        WHEN zif_sat_c_object_browser=>c_search_option-by_api.
          DATA(lv_api_state_table) = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_apistates }| ).
          add_join_table(
              iv_join_table = |{ lv_api_state_table }|
              iv_alias      = c_api_alias
              it_conditions = VALUE #(
                 ( field = 'objectname' ref_field = 'ddlname' ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field and_or = zif_sat_c_selection_condition=>and )
                 ( field = 'objecttype' tabname_alias = CONV #( c_api_alias ) value = 'DDLS' type = zif_sat_c_join_cond_type=>filter )
              )
          ).
          add_api_option_filter( it_values = <ls_option>-value_range ).

*.......... Find views where the filter exists in the FROM part of the cds view
        WHEN zif_sat_c_object_browser=>c_search_option-by_select_from.
          add_from_option_filter( <ls_option>-value_range ).

*.......... Find views which have a certain annotation
        WHEN zif_sat_c_object_browser=>c_search_option-by_anno.
          add_anno_option_filter(
            it_values = <ls_option>-value_range
          ).

*.......... Find views that are parameterized
        WHEN zif_sat_c_object_browser=>c_search_option-by_params.
          CHECK <ls_option>-value_range IS NOT INITIAL.
          DATA(lf_views_with_parameters) = xsdbool( <ls_option>-value_range[ 1 ]-low = 'TRUE' ).
          IF lf_views_with_parameters = abap_true.
            add_join_table(
                iv_join_table = |{ get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsviewwithparameter }| ) }|
                iv_alias      = c_parameterized_view_alias
                it_conditions = VALUE #(
                  ( field = CONV #( c_fields-entityid ) ref_field = c_fields-entityid ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field )
                )
            ).
          ELSE.
            add_subquery_filter(
                iv_fieldname = |{ c_base_alias }~{ c_fields-entityid }|
                iv_subquery  = gv_params_subquery
                iv_option    = zif_sat_c_options=>not_in_subquery
            ).
          ENDIF.

*.......... Find views that have a certain parameter
        WHEN zif_sat_c_object_browser=>c_search_option-by_param.
          add_param_option_filter( <ls_option>-value_range ).

*.......... Find views which have a certain field a component
        WHEN zif_sat_c_object_browser=>c_search_option-by_field.
          add_field_option_filter( <ls_option>-value_range ).

*.......... Find views where an entity is used as an association
        WHEN zif_sat_c_object_browser=>c_search_option-by_association.
          add_association_option_filter( <ls_option>-value_range ).

*.......... Find views for a certain type e.g. function, hierarchy, view
        WHEN zif_sat_c_object_browser=>c_search_option-by_type.
          add_type_option_filter( it_values = <ls_option>-value_range ).
      ENDCASE.
    ENDLOOP.

    new_and_cond_list( ).

    search( ).

    IF mt_result IS NOT INITIAL AND mo_search_query->ms_search_engine_params-with_api_state = abap_true.
      add_api_state_info( ).
    ENDIF.

    rt_result = mt_result.
  ENDMETHOD.


  METHOD  determine_grouping.
    CHECK mo_search_query->ms_search_engine_params-use_and_cond_for_options = abap_true.

****.. Excluding would break the relational division logic and would lead to unreliable results
****    CHECK mf_excluding_found = abap_false.
    IF NOT ( mv_anno_filter_count > 1 OR
             mv_field_filter_count > 1 OR
             mv_assoc_filter_count > 1 OR
             mv_from_filter_count > 1 OR
             mv_param_filter_count > 1 ).
      RETURN.
    ENDIF.

*.. Create grouping clause
    add_group_by_clause( |{ c_base_alias }~{ c_fields-entityid }| ).
    add_group_by_clause( |{ c_base_alias }~rawentityid| ).
    add_group_by_clause( |{ c_base_alias }~sourcetype| ).
    add_group_by_clause( |{ c_base_alias }~ddlname| ).
    add_group_by_clause( |{ c_base_alias }~description| ).
    add_group_by_clause( |{ c_base_alias }~createdby| ).
    add_group_by_clause( |{ c_base_alias }~developmentpackage| ).

    IF mv_anno_filter_count > 1.
      add_having_clause( iv_field = |{ c_anno_alias }~{ c_fields-name }| iv_counter_compare = mv_anno_filter_count ).
    ENDIF.

    IF mv_assoc_filter_count > 1.
      add_having_clause( iv_field = |{ c_used_in_association_alias }~usedentity| iv_counter_compare = mv_assoc_filter_count ).
    ENDIF.

    IF mv_from_filter_count > 1.
      add_having_clause( iv_field = |{ c_select_from_alias }~sourceentity| iv_counter_compare = mv_from_filter_count ).
    ENDIF.

    IF mv_field_filter_count > 1.
      add_having_clause( iv_field = |{ c_fields-alias }~fieldname| iv_counter_compare = mv_field_filter_count ).
    ENDIF.

    IF mv_param_filter_count > 1.
      add_having_clause( iv_field = |{ c_param_alias }~parametername| iv_counter_compare = mv_param_filter_count ).
    ENDIF.

  ENDMETHOD.

  METHOD add_param_option_filter.
    split_including_excluding(
        EXPORTING it_values    = it_values
        IMPORTING et_including = DATA(lt_including)
                  et_excluding = DATA(lt_excluding)
    ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter(
          iv_subquery_fieldname = 'parametername'
          iv_fieldname          = |{ c_base_alias }~entityid|
          it_excluding          = lt_excluding
          iv_subquery           = gv_param_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = CONV #( zif_sat_c_select_source_id=>dd10b )
          iv_alias      = c_param_alias
          it_conditions = VALUE #( ( field = 'strucobjn' ref_field = c_fields-entityid ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field ) )
      ).
      add_option_filter(
          iv_fieldname = |{ c_param_alias }~parametername|
          it_values    = lt_including
      ).
      mv_param_filter_count = lines( lt_including ).
    ENDIF.

  ENDMETHOD.

  METHOD add_anno_option_filter.
    DATA: lt_or_seltab TYPE zif_sat_ty_global=>ty_t_or_seltab_sql.


    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding)
    ).

*.. Create sub query for negated annotation key/value pairs
    IF lt_excluding IS NOT INITIAL.
      LOOP AT lt_excluding ASSIGNING FIELD-SYMBOL(<ls_excluding>).
        DATA(lt_and_seltab) = VALUE zif_sat_ty_global=>ty_t_seltab_sql(
           ( sqlfieldname = c_fields-name
             sign         = zif_sat_c_options=>including
             option       = <ls_excluding>-option
             low          = <ls_excluding>-low )
        ).
        IF <ls_excluding>-high IS NOT INITIAL.
          lt_and_seltab = VALUE #(
            BASE lt_and_seltab
            ( sqlfieldname = c_fields-value
              sign         = zif_sat_c_options=>including
              option       = <ls_excluding>-option2
              low          = <ls_excluding>-high )
          ).
        ENDIF.
        lt_or_seltab = VALUE #( BASE lt_or_seltab ( values = lt_and_seltab ) ).
      ENDLOOP.

      create_not_in_filter_for_where(
          it_where     = zcl_sat_where_clause_builder=>create_or_condition( lt_or_seltab )
          iv_fieldname = |{ c_base_alias }~{ c_fields-entityid }|
          iv_subquery  = gv_anno_subquery
      ).
    ENDIF.

*.. Add filters for including annotation key/value pairs
    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = |{ get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsannotation }| ) }|
          iv_alias      = c_anno_alias
          it_conditions = VALUE #(
            ( field = c_fields-entityid ref_field = c_fields-entityid ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field )
          )
      ).
      mv_anno_filter_count = lines( lt_including ).
      new_and_cond_list( ).
      LOOP AT lt_including INTO DATA(ls_filter_value).
        add_filter( VALUE #( sqlfieldname = |{ c_anno_alias }~{ c_fields-name }|
                             sign         = ls_filter_value-sign
                             option       = ls_filter_value-option
                             low          = ls_filter_value-low )  ).

        IF ls_filter_value-high IS NOT INITIAL.
          add_filter( VALUE #( sqlfieldname = |{ c_anno_alias }~{ c_fields-value }|
                               sign         = ls_filter_value-sign2
                               option       = ls_filter_value-option2
                               low          = ls_filter_value-high )  ).
        ENDIF.
        new_or_cond_list( ).
      ENDLOOP.

      new_and_cond_list( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_api_option_filter.
    DATA: lt_api_filters   TYPE zif_sat_ty_object_browser=>ty_t_value_range,
          lt_state_filters TYPE zif_sat_ty_object_browser=>ty_t_value_range.

    LOOP AT it_values INTO DATA(ls_value).

      CASE ls_value-low.

        WHEN zif_sat_c_object_browser=>c_api_option_value-released OR
             zif_sat_c_object_browser=>c_api_option_value-deprecated.
          lt_state_filters = VALUE #( BASE lt_state_filters ( ls_value ) ).

        WHEN OTHERS.
          lt_api_filters = VALUE #( BASE lt_api_filters ( ls_value ) ).
      ENDCASE.

    ENDLOOP.

    IF lt_api_filters IS NOT INITIAL.

      add_option_filter(
          iv_fieldname = |{ c_api_alias }~filtervalue|
          it_values    = lt_api_filters
      ).
    ENDIF.

    IF lt_state_filters IS NOT INITIAL.
      add_option_filter(
          iv_fieldname = |{ c_api_alias }~apistate|
          it_values    = lt_state_filters
      ).
    ENDIF.
  ENDMETHOD.


  METHOD add_type_option_filter.
    DATA: lt_type_filters TYPE zif_sat_ty_object_browser=>ty_t_value_range.

    LOOP AT it_values INTO DATA(ls_value).
      CASE ls_value-low.

        WHEN zif_sat_c_object_browser=>c_type_option_value-extend.
          ls_value-low = zif_sat_c_cds_view_type=>extend.

        WHEN zif_sat_c_object_browser=>c_type_option_value-function.
          ls_value-low = zif_sat_c_cds_view_type=>table_function.

        WHEN zif_sat_c_object_browser=>c_type_option_value-hierarchy.
          ls_value-low = zif_sat_c_cds_view_type=>hierarchy.

        WHEN zif_sat_c_object_browser=>c_type_option_value-view.
          ls_value-low = zif_sat_c_cds_view_type=>view.

        WHEN zif_sat_c_object_browser=>c_type_option_value-abstract_entity.
          ls_value-low = zif_sat_c_cds_view_type=>abstract_entity.

        WHEN zif_sat_c_object_browser=>c_type_option_value-custom_entity.
          ls_value-low = zif_sat_c_cds_view_type=>custom_entity.
      ENDCASE.

      lt_type_filters = VALUE #( BASE lt_type_filters ( ls_value ) ).
    ENDLOOP.

    add_option_filter(
        iv_fieldname = 'sourcetype'
        it_values    = lt_type_filters
    ).
  ENDMETHOD.


  METHOD add_association_option_filter.
    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter(
          iv_subquery_fieldname = 'usedentity'
          iv_fieldname          = |{ c_base_alias }~ddlname|
          it_excluding          = lt_excluding
          iv_subquery           = gv_assoc_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      DATA(lv_association_usage_table) = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_associatedincds }| ).
      add_join_table(
          iv_join_table = |{ lv_association_usage_table }|
          iv_alias      = c_used_in_association_alias
          it_conditions = VALUE #(
            ( field = 'ddlname' ref_field = 'ddlname' ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field )
          )
      ).

      add_option_filter(
          iv_fieldname = |{ c_used_in_association_alias }~usedentity|
          it_values    = it_values
      ).
      mv_assoc_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_field_option_filter.

    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).
    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter(
          iv_subquery_fieldname = 'fieldname'
          iv_fieldname          = |{ c_base_alias }~{ c_fields-entityid }|
          it_excluding          = lt_excluding
          iv_subquery           = gv_field_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = |{ get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsviewfield }| ) }|
          iv_alias      = c_fields-alias
          it_conditions = VALUE #(
            ( field = c_fields-entityid ref_field = c_fields-entityid ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field )
          )
      ).
      add_option_filter(
          iv_fieldname = |{ c_fields-alias }~fieldname|
          it_values    = lt_including
      ).
      mv_field_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_from_option_filter.
    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter(
          iv_subquery_fieldname = 'sourceentity'
          iv_fieldname          = |{ c_base_alias }~viewname|
          it_excluding          = lt_excluding
          iv_subquery           = gv_select_from_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      DATA(lv_from_part_table) = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity }| ).
      add_join_table(
          iv_join_table = |{ lv_from_part_table }|
          iv_alias      = c_select_from_alias
          it_conditions = VALUE #( ( field = 'ddlviewname' ref_field = 'viewname' ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field ) )
      ).
      add_option_filter(
          iv_fieldname = |{ c_select_from_alias }~sourceentity|
          it_values    = lt_including
      ).
      mv_from_filter_count = lines( lt_including ).
    ENDIF.

  ENDMETHOD.


  METHOD add_extensions_filter.
    DATA(lv_and_or) = ``.
    DATA(lt_ext_join_filter) = VALUE zsat_join_condition_data_t( ).

    LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_value_range>).

      lt_ext_join_filter = VALUE #( BASE lt_ext_join_filter
        ( field         = c_fields-entityid
          operator      = COND #( WHEN <ls_value_range>-option = 'CP' THEN zif_sat_c_operator=>like ELSE zif_sat_c_operator=>equals )
          value         = <ls_value_range>-low
          value_type    = zif_sat_c_join_cond_val_type=>typed_input
          tabname_alias = c_extension_view_alias
          and_or        = lv_and_or )
      ).
      lv_and_or = zif_sat_c_selection_condition=>or.
    ENDLOOP.

    add_join_table(
        iv_join_table = |{ get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsextensionviews }| ) }|
        iv_alias      = c_extension_view_alias
        it_conditions = VALUE #(
          ( field = 'parentddl' ref_field = 'ddlname' ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field and_or = zif_sat_c_selection_condition=>and )
          ( LINES OF lt_ext_join_filter )
        )
    ).
  ENDMETHOD.


  METHOD add_api_state_info.
    SELECT DISTINCT
           objectname AS entity_id,
           apistate AS api_state
      FROM zsat_i_apistates
      FOR ALL ENTRIES IN @mt_result
      WHERE objectname = @mt_result-secondary_entity_id
        AND filtervalue <> @zif_sat_c_cds_api_state=>add_custom_fields
    INTO TABLE @DATA(lt_api_states).

    CHECK sy-subrc = 0.

    LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      <ls_result>-api_state = VALUE #( lt_api_states[ entity_id = <ls_result>-secondary_entity_id ]-api_state OPTIONAL ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_search_terms_to_search.
    DEFINE _add_filter.
      add_filter( VALUE #( sqlfieldname = |{ c_base_alias }~&1|
                           option       = &2-option
                           sign         = &2-sign
                           low          = &2-low ) ).
    END-OF-DEFINITION.

    DEFINE _new_cond_list.
      IF &1 = abap_true.
        new_and_cond_list( ).
      ELSE.
        new_or_cond_list( ).
      ENDIF.
    END-OF-DEFINITION.

*.. If at least one search term with 'E'(Excluding) sign exists all search terms are connected
*.... via AND in the SQL Clause
    IF line_exists( mo_search_query->mt_search_term[ sign = zif_sat_c_options=>excluding ] ).
      DATA(lf_use_and_between_terms) = abap_true.
    ENDIF.

    new_and_cond_list( ).

    LOOP AT mo_search_query->mt_search_term ASSIGNING FIELD-SYMBOL(<ls_term>).
      DATA(lf_and) = xsdbool( <ls_term>-sign = zif_sat_c_options=>excluding ).

      _add_filter entityid <ls_term>.
      _new_cond_list lf_and.

      _add_filter ddlname <ls_term>.
      _new_cond_list lf_and.

      _add_filter viewname <ls_term>.

      IF sy-tabix <> lines( mo_search_query->mt_search_term ).
        _new_cond_list lf_use_and_between_terms.
      ENDIF.
    ENDLOOP.

    new_and_cond_list( ).

  ENDMETHOD.

ENDCLASS.
