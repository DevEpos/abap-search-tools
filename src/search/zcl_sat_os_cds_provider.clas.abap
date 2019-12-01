"! <p class="shorttext synchronized" lang="en">CDS View Search Provider for Object Search</p>
CLASS zcl_sat_os_cds_provider DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_base_search_provider.

  PUBLIC SECTION.
    INTERFACES zif_sat_ty_object_search.

    ALIASES:
      ty_t_value_range FOR zif_sat_ty_object_search~ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor.
  PROTECTED SECTION.
    METHODS determine_grouping
        REDEFINITION.
    METHODS prepare_search
        REDEFINITION.
  PRIVATE SECTION.
    ALIASES:
      c_cds_search_params FOR zif_sat_c_object_search~c_cds_search_params.

    DATA mv_field_subquery TYPE string.
    DATA mv_anno_subquery TYPE string.
    DATA mv_select_from_subquery TYPE string.
    DATA mv_assoc_subquery TYPE string.
    DATA mv_param_subquery TYPE string.
    DATA mv_params_subquery TYPE string.

    CONSTANTS:
      c_base_alias                TYPE string VALUE 'base' ##NO_TEXT,
      c_anno_alias                TYPE string VALUE 'anno' ##NO_TEXT,
      c_extension_view_alias      TYPE string VALUE 'ext' ##no_text,
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

    "! <p class="shorttext synchronized" lang="en">Create filter for ANNO option</p>
    "!
    METHODS add_anno_option_filter
      IMPORTING
        it_values TYPE ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for PARAM option</p>
    "!
    METHODS add_param_option_filter
      IMPORTING
        it_values TYPE ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FIELD option</p>
    METHODS add_field_option_filter
      IMPORTING
        it_values TYPE ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for ASSOC option</p>
    METHODS add_association_option_filter
      IMPORTING
        it_values TYPE ty_t_value_range.
    "! <p class="shorttext synchronized" lang="en">Create filter for FROM option</p>
    METHODS add_from_option_filter
      IMPORTING
        it_values TYPE ty_t_value_range.	
    "! <p class="shorttext synchronized" lang="en">Adds extensions filter to query</p>
    METHODS add_extensions_filter
      IMPORTING
        it_values TYPE ty_t_value_range.
ENDCLASS.



CLASS zcl_sat_os_cds_provider IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
*.. Create sub queries for parameters where boolean operation AND is senseful
    mv_field_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                        | FROM { get_cds_sql_name( conv #( zif_sat_c_select_source_id=>zsat_i_cdsviewfield ) ) } | && c_cr_lf &&
                        | WHERE |.
    mv_anno_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                       | FROM { get_cds_sql_name( conv #( zif_sat_c_select_source_id=>zsat_i_cdsannotation ) ) } | && c_cr_lf &&
                       | WHERE |.
    mv_assoc_subquery = |SELECT DISTINCT ddlname | && c_cr_lf &&
                        | FROM { get_cds_sql_name( CONV #( zif_sat_c_select_source_id=>zsat_i_associatedincds ) ) } | && c_cr_lf &&
                        | WHERE |.
    mv_select_from_subquery = |SELECT DISTINCT ddlviewname | && c_cr_lf &&
                              | FROM { get_cds_sql_name( CONV #( zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity ) ) } | && c_cr_lf &&
                              | WHERE |.
    mv_param_subquery = |SELECT DISTINCT strucobjn | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>dd10b } | && c_cr_lf &&
                        | WHERE |.
    mv_params_subquery = |SELECT DISTINCT entityid | && c_cr_lf &&
                         | FROM { get_cds_sql_name( CONV #( zif_sat_c_select_source_id=>zsat_i_cdsviewwithparameter ) ) }|.
  ENDMETHOD.

  METHOD prepare_search.

    set_base_select_table(
        iv_entity = get_cds_sql_name( CONV #( zif_sat_c_select_source_id=>zsat_i_cdsentity ) )
        iv_alias  = c_base_alias
        it_parameters = value #( ( param_name = 'p_language' param_value = sy-langu ) )
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
      add_search_terms_to_search( VALUE #( ( |{ c_base_alias }~entityid| )
                                           ( |{ c_base_alias }~ddlname| )
                                           ( |{ c_base_alias }~viewname| ) ) ).
    ENDIF.

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).
      CASE <ls_option>-option.

*.......... Find views which have a certain extension
        WHEN c_cds_search_params-extended_by.
          add_extensions_filter( EXPORTING it_values = <ls_option>-value_range ).

*.......... Find views via its description
        WHEN c_general_search_options-description.
          add_option_filter(
            iv_fieldname = mv_description_filter_field
            it_values    = <ls_option>-value_range
          ).

*.......... Find views with a certain responsible person
        WHEN c_general_search_options-user.
          add_option_filter(
            iv_fieldname = 'createdby'
            it_values    = <ls_option>-value_range
          ).

*.......... Find views which exist in a certain development package
        WHEN c_general_search_options-package.
          add_option_filter(
            iv_fieldname = 'developmentpackage'
            it_values    = <ls_option>-value_range
          ).

*.......... Find views where the filter exists in the FROM part of the cds view
        WHEN c_cds_search_params-select_from.
          add_from_option_filter( <ls_option>-value_range ).

*.......... Find views which have a certain annotation
        WHEN c_cds_search_params-annotation.
          add_anno_option_filter(
            it_values = <ls_option>-value_range
          ).

*.......... Find views that are parameterized
        WHEN c_cds_search_params-params.
          CHECK <ls_option>-value_range IS NOT INITIAL.
          DATA(lf_views_with_parameters) = xsdbool( <ls_option>-value_range[ 1 ]-low = 'TRUE' ).
          IF lf_views_with_parameters = abap_true.
            add_join_table(
                iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsviewwithparameter }| )
                iv_alias      = c_parameterized_view_alias
                it_conditions = VALUE #(
                  ( field = CONV #( c_fields-entityid ) ref_field = c_fields-entityid ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field )
                )
            ).
          ELSE.
            add_subquery_filter(
                iv_fieldname = |{ c_base_alias }~{ c_fields-entityid }|
                iv_subquery  = mv_params_subquery
                iv_option    = zif_sat_c_options=>not_in_subquery
            ).
          ENDIF.

*.......... Find views that have a certain parameter
        WHEN c_cds_search_params-param.
          add_param_option_filter( <ls_option>-value_range ).

*.......... Find views which have a certain field a component
        WHEN c_cds_search_params-field.
          add_field_option_filter( <ls_option>-value_range ).

*.......... Find views where an entity is used as an association
        WHEN c_cds_search_params-association.
          add_association_option_filter( <ls_option>-value_range ).

*.......... Find views for a certain type e.g. function, hierarchy, view
        WHEN c_general_search_options-type.
          add_option_filter(
              iv_fieldname = 'sourcetype'
              it_values    = <ls_option>-value_range
          ).
      ENDCASE.
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.


  METHOD  determine_grouping.
    CHECK ms_search_engine_params-use_and_cond_for_options = abap_true.

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
          iv_subquery           = mv_param_subquery
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
          iv_subquery  = mv_anno_subquery
      ).
    ENDIF.

*.. Add filters for including annotation key/value pairs
    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsannotation }| )
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
          iv_subquery           = mv_assoc_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_associatedincds }| )
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
          iv_subquery           = mv_field_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsviewfield }| )
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
          iv_subquery           = mv_select_from_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsfrompartentity }| )
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
          and_or        = lv_and_or
          type          = zif_sat_c_join_cond_type=>filter )
      ).
      lv_and_or = zif_sat_c_selection_condition=>or.
    ENDLOOP.

    add_join_table(
        iv_join_table = get_cds_sql_name( |{ zif_sat_c_select_source_id=>zsat_i_cdsextensionviews }| )
        iv_alias      = c_extension_view_alias
        it_conditions = VALUE #(
          ( field = 'parentddl' ref_field = 'ddlname' ref_table_alias = c_base_alias type = zif_sat_c_join_cond_type=>field and_or = zif_sat_c_selection_condition=>and )
          ( LINES OF lt_ext_join_filter )
        )
    ).
  ENDMETHOD.


ENDCLASS.
