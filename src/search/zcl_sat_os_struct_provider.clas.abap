"! <p class="shorttext synchronized">Structure search provider for Object search</p>
CLASS zcl_sat_os_struct_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_os_tabl_options.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor.

  PROTECTED SECTION.
    METHODS prepare_search       REDEFINITION.
    METHODS is_grouping_required REDEFINITION.
    METHODS add_having_clauses   REDEFINITION.
    METHODS do_after_search      REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_tabl_search_params FOR zif_sat_c_os_tabl_options~c_filter_key.

    CONSTANTS c_base_table TYPE string VALUE 'base'.
    CONSTANTS c_field_table TYPE string VALUE 'field'.
    CONSTANTS c_text_table TYPE string VALUE 'text'.
    CONSTANTS c_include_table TYPE string VALUE 'incl_usage'.
    CONSTANTS c_tech_settings_table TYPE string VALUE 'tech'.
    CONSTANTS:
      BEGIN OF c_fields,
        alias               TYPE string VALUE 'field',
        tabname             TYPE string VALUE 'tablename',
        structure           TYPE string VALUE 'structure',
        type                TYPE string VALUE 'type',
        fieldname           TYPE string VALUE 'fieldname',
        include_name        TYPE string VALUE 'includename',
        description         TYPE string VALUE 'description',
        development_package TYPE string VALUE 'developmentpackage',
        created_by          TYPE string VALUE 'createdby',
        created_date        TYPE string VALUE 'createddate',
        changed_by          TYPE string VALUE 'changedby',
        changed_date        TYPE string VALUE 'changeddate',
        extension_class     TYPE string VALUE 'extensionclass',
        language            TYPE string VALUE 'language',
      END OF c_fields.

    DATA mv_field_subquery TYPE string.
    DATA mv_field_filter_count TYPE i.
    DATA mv_incl_filter_count TYPE i.
    DATA mf_dd09l_join_needed TYPE abap_bool.

    "! <p class="shorttext synchronized">Create filter for FIELD option</p>
    METHODS add_field_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_include_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS configure_filters.
ENDCLASS.


CLASS zcl_sat_os_struct_provider IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_field_subquery = |SELECT DISTINCT tablename { c_cr_lf }| &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_tablefield } { c_cr_lf }| &&
                        | WHERE |.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_structure
                           iv_alias  = c_base_table ).

    add_select_field( iv_fieldname       = c_fields-structure
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-structure
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_base_table
                      if_no_grouping     = abap_true  ).
    add_select_field( iv_fieldname       = c_fields-created_by
                      iv_fieldname_alias = c_result_fields-created_by
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-created_date
                      iv_fieldname_alias = c_result_fields-created_date
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-changed_by
                      iv_fieldname_alias = c_result_fields-changed_by
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-changed_date
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-development_package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = |'{ zif_sat_c_tadir_types=>table }'|
                      iv_fieldname_alias = c_result_fields-tadir_type
                      if_no_grouping     = abap_true ).

    add_order_by( iv_fieldname = c_fields-structure
                  iv_entity    = c_base_table  ).

    add_search_terms_to_search( iv_target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_fields = VALUE #( ( fieldname =  |{ c_base_table }~{ c_fields-structure }| ) ) ).

    configure_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
    fill_descriptions( ).
  ENDMETHOD.

  METHOD is_grouping_required.
    result = super->is_grouping_required( ).
    IF result = abap_true.
      RETURN.
    ENDIF.

    IF    mv_field_filter_count > 1
       OR mv_incl_filter_count  > 1.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_having_clauses.
    IF mv_field_filter_count > 1.
      add_having_clause( iv_field           = |{ c_field_table }~{ c_fields-fieldname }|
                         iv_counter_compare = mv_field_filter_count ).
    ENDIF.

    IF mv_incl_filter_count > 1.
      add_having_clause( iv_field           = |{ c_include_table }~{ c_fields-include_name }|
                         iv_counter_compare = mv_incl_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD configure_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

        WHEN c_general_search_options-description.
          add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tabletext }|
                          iv_alias      = c_text_table
                          it_conditions = VALUE #( ( field           = c_fields-tabname
                                                     ref_field       = c_fields-structure
                                                     ref_table_alias = c_base_table
                                                     type            = zif_sat_c_join_cond_type=>field  )
                                                   ( field           = c_fields-language
                                                     tabname_alias   = c_text_table
                                                     value           = sy-langu
                                                     type            = zif_sat_c_join_cond_type=>filter  ) ) ).
          add_option_filter( iv_fieldname = |{ c_text_table }~{ mv_description_filter_field }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = c_fields-created_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = c_fields-created_date
                           it_values    = <ls_option>-value_range ).
        WHEN c_general_search_options-package.
          add_package_filter( iv_fieldname = |{ c_base_table }~{ c_fields-development_package }|
                              it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-software_component.
          add_softw_comp_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-development_package )
                                 iv_ref_table_alias = c_base_table ).

        WHEN c_general_search_options-application_component.
          add_appl_comp_filter( it_values          = <ls_option>-value_range
                                iv_ref_field       = CONV #( c_fields-development_package )
                                iv_ref_table_alias = c_base_table ).

        WHEN c_tabl_search_params-field.
          add_field_filter( <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = c_fields-changed_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = c_fields-changed_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-release_state.
          add_api_option_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-structure )
                                 iv_ref_table_alias = c_base_table
                                 it_tadir_type      = VALUE #( ( zif_sat_c_tadir_types=>table ) ) ).

        WHEN c_tabl_search_params-enhancement_category.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-extension_class }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_tabl_search_params-include_usage.
          add_include_filter( it_values = <ls_option>-value_range  ).

        WHEN c_general_search_options-type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-type }|
                             it_values    = <ls_option>-value_range ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_field_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-fieldname
                            iv_fieldname          = |{ c_base_table }~{ c_fields-structure }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_field_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      set_distinct_required( ).
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tablefield }|
                      iv_alias      = |{ c_field_table }|
                      it_conditions = VALUE #( ( field           = c_fields-tabname
                                                 ref_field       = c_fields-structure
                                                 ref_table_alias = c_base_table
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).

      add_option_filter( iv_fieldname = |{ c_field_table }~{ c_fields-fieldname }|
                         it_values    = lt_including ).

      mv_field_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_include_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-include_name
                            iv_fieldname          = |{ c_base_table }~{ c_fields-structure }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_field_subquery ).
    ENDIF.

    IF lt_including IS INITIAL.
      RETURN.
    ENDIF.

    set_distinct_required( ).
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tablefield }|
                    iv_alias      = |{ c_include_table }|
                    it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                             ( field           = c_fields-tabname
                                               ref_field       = c_fields-structure
                                               ref_table_alias = c_base_table
                                               type            = zif_sat_c_join_cond_type=>field )
                                             ( field           = c_fields-fieldname
                                               tabname_alias   = c_include_table
                                               value           = '.INCLUDE'
                                               type            = zif_sat_c_join_cond_type=>filter ) ) ).

    add_option_filter( iv_fieldname = |{ c_include_table }~{ c_fields-include_name }|
                       it_values    = lt_including ).

    mv_incl_filter_count = lines( lt_including ).
  ENDMETHOD.
ENDCLASS.
