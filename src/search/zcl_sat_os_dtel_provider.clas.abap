"! <p class="shorttext synchronized">Object search provider for DTEL</p>
CLASS zcl_sat_os_dtel_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS prepare_search       REDEFINITION.
    METHODS do_after_search      REDEFINITION.
    METHODS is_grouping_required REDEFINITION.
    METHODS add_having_clauses   REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_base_table TYPE string VALUE 'base'.
    CONSTANTS c_text_table TYPE string VALUE 'text'.
    CONSTANTS c_table_field_table TYPE string VALUE 'table'.

    CONSTANTS: BEGIN OF c_fields,
                 tablename                TYPE string VALUE 'tablename',
                 rollname                 TYPE string VALUE 'rollname',
                 language                 TYPE string VALUE 'language',
                 longtext                 TYPE string VALUE 'longtextupper',
                 mediumtext               TYPE string VALUE 'mediumtextupper',
                 shorttext                TYPE string VALUE 'shorttextupper',
                 reptext                  TYPE string VALUE 'reptextupper',
                 " type used (domain, referenced ddic, referenced clas/intf)
                 type_category            TYPE string VALUE 'typecategory',
                 " Type of reference type (table type, class, interface)
                 ref_type                 TYPE string VALUE 'reftype',
                 domname                  TYPE string VALUE 'domname',
                 set_get_param            TYPE string VALUE 'setgetparamid',
                 shlpname                 TYPE string VALUE 'shlpname',
                 shlpfield                TYPE string VALUE 'shlpfield',
                 defaultcompname          TYPE string VALUE 'defaultcompname',
                 data_type                TYPE string VALUE 'datatype',
                 length                   TYPE string VALUE 'length',
                 decimals                 TYPE string VALUE 'decimals',
                 search_help              TYPE string VALUE 'shlpname',
                 search_help_param        TYPE string VALUE 'shlpfield',
                 is_changedoc_enabled     TYPE string VALUE 'ischangedocenabled',
                 is_basic_write_dir_ltr   TYPE string VALUE 'isbasicwritedirltr',
                 is_bidi_filter_enabled   TYPE string VALUE 'isbidifilterenabled',
                 is_input_history_enabled TYPE string VALUE 'isinputhistoryenabled',
                 development_package      TYPE string VALUE 'developmentpackage',
                 created_by               TYPE string VALUE 'createdby',
                 created_date             TYPE string VALUE 'createddate',
                 changed_by               TYPE string VALUE 'changedby',
                 changed_date             TYPE string VALUE 'changeddate',
               END OF c_fields.

    DATA mf_text_table_join TYPE abap_bool.
    DATA mv_table_subquery TYPE string.
    DATA mv_table_filter_count TYPE i.

    METHODS configure_filters.
    METHODS add_text_table_join.

    METHODS add_label_filters
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_table_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_flag_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS map_flag_opt_to_field
      IMPORTING
        iv_option     TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sat_os_dtel_provider IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_table_subquery = |SELECT DISTINCT rollname { c_cr_lf }| &&
                                 | FROM { zif_sat_c_select_source_id=>zsat_i_tablefield } { c_cr_lf }| &&
                                 | WHERE |.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_dataelement
                           iv_alias  = c_base_table ).

    add_select_field( iv_fieldname       = c_fields-rollname
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-rollname
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
    add_select_field( iv_fieldname       = |'{ zif_sat_c_tadir_types=>data_element }'|
                      iv_fieldname_alias = c_result_fields-tadir_type
                      if_no_grouping     = abap_true ).

    add_order_by( iv_fieldname = c_fields-rollname
                  iv_entity    = c_base_table  ).

    add_search_terms_to_search( iv_target = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_fields = VALUE #( ( fieldname =  |{ c_base_table }~{ c_fields-rollname }| ) ) ).

    configure_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD configure_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

        WHEN c_general_search_options-description.
          add_text_table_join( ).
          add_option_filter( iv_fieldname = |{ c_text_table }~{ mv_description_filter_field }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-label.
          add_text_table_join( ).
          add_label_filters( it_values = <ls_option>-value_range ).

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

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = c_fields-changed_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = c_fields-changed_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-release_state.
          add_api_option_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-rollname )
                                 iv_ref_table_alias = c_base_table
                                 it_tadir_type      = VALUE #( ( zif_sat_c_tadir_types=>data_element ) ) ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-length.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-length }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-table.
          add_table_filter( it_values = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-data_type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-data_type }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-type_category.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-type_category }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-ref_type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-ref_type }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_object_search=>c_general_search_params-type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-domname }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-param.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-set_get_param }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-default_component.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-defaultcompname }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-flag.
          add_flag_filter( it_values = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-shlp_name.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-search_help }|
                             it_values    = <ls_option>-value_range ).

        WHEN zif_sat_c_os_dtel_options=>c_filter_key-shlp_param.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-search_help_param }|
                             it_values    = <ls_option>-value_range ).

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_grouping_required.
    result = super->is_grouping_required( ).
    IF result = abap_true.
      RETURN.
    ENDIF.

    IF mv_table_filter_count > 1.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD add_having_clauses.
    IF mv_table_filter_count > 1.
      add_having_clause( iv_field           = |{ c_table_field_table }~{ c_fields-tablename }|
                         iv_counter_compare = mv_table_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD do_after_search.
    fill_descriptions( ).
  ENDMETHOD.

  METHOD add_text_table_join.
    CHECK mf_text_table_join = abap_false.

    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_dataelementtext }|
                    iv_alias      = c_text_table
                    it_conditions = VALUE #( ( field           = c_fields-rollname
                                               ref_field       = c_fields-rollname
                                               ref_table_alias = c_base_table
                                               type            = zif_sat_c_join_cond_type=>field  )
                                             ( field           = c_fields-language
                                               tabname_alias   = c_text_table
                                               value           = sy-langu
                                               type            = zif_sat_c_join_cond_type=>filter  ) ) ).

    mf_text_table_join = abap_true.
  ENDMETHOD.

  METHOD add_label_filters.
    CHECK it_values IS NOT INITIAL.

    new_and_cond_list( ).

    IF line_exists( it_values[ sign = zif_sat_c_options=>excluding ] ).
      DATA(lf_use_and_between_terms) = abap_true.
    ENDIF.

    LOOP AT it_values INTO DATA(ls_value).
      DATA(lv_tabix) = sy-tabix.
      DATA(lf_and) = xsdbool( ls_value-sign = zif_sat_c_options=>excluding ).
      add_filter( VALUE #( low    = CONV scrtext_s( ls_value-low )
                           option = ls_value-option
                           sign   = ls_value-sign
                           field  = c_fields-shorttext ) ).
      new_cond_list( lf_and ).
      add_filter( VALUE #( low    = CONV scrtext_m( ls_value-low )
                           option = ls_value-option
                           sign   = ls_value-sign
                           field  = c_fields-mediumtext ) ).
      new_cond_list( lf_and ).
      add_filter( VALUE #( low    = CONV scrtext_l( ls_value-low )
                           option = ls_value-option
                           sign   = ls_value-sign
                           field  = c_fields-longtext ) ).
      new_cond_list( lf_and ).
      add_filter( VALUE #( low    = CONV reptext( ls_value-low )
                           option = ls_value-option
                           sign   = ls_value-sign
                           field  = c_fields-reptext ) ).

      IF lv_tabix <> lines( it_values ).
        IF lf_use_and_between_terms = abap_true.
          new_and_cond_list( ).
        ELSE.
          new_or_cond_list( ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD add_table_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = c_fields-tablename
                            iv_fieldname          = |{ c_base_table }~{ c_fields-rollname }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_table_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      set_distinct_required( ).
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_tablefield }|
                      iv_alias      = |{ c_table_field_table }|
                      it_conditions = VALUE #( ( field           = c_fields-rollname
                                                 ref_field       = c_fields-rollname
                                                 ref_table_alias = c_base_table
                                                 type            = zif_sat_c_join_cond_type=>field ) ) ).

      add_option_filter( iv_fieldname = |{ c_table_field_table }~{ c_fields-tablename }|
                         it_values    = lt_including ).

      mv_table_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_flag_filter.
    DATA ls_value TYPE zif_sat_ty_object_search=>ty_s_value_range.

    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).

    LOOP AT lt_excluding INTO ls_value.
      add_option_filter( iv_fieldname = map_flag_opt_to_field( ls_value-low )
                         it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = abap_false ) ) ).
    ENDLOOP.

    IF lt_including IS INITIAL.
      RETURN.
    ENDIF.

    IF ms_search_engine_params-use_and_cond_for_options = abap_true.
      LOOP AT lt_including INTO ls_value.
        add_option_filter( iv_fieldname = map_flag_opt_to_field( ls_value-low )
                           it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ) ).
      ENDLOOP.
    ELSE.
      new_and_cond_list( ).

      LOOP AT lt_including INTO ls_value.
        add_option_filter( iv_fieldname = map_flag_opt_to_field( ls_value-low )
                           it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ) ).
        new_or_cond_list( ).
      ENDLOOP.

      new_and_cond_list( ).
    ENDIF.
  ENDMETHOD.

  METHOD map_flag_opt_to_field.
    result = SWITCH string( iv_option
                            WHEN zif_sat_c_os_dtel_options=>c_flag-basic_dir_is_ltr THEN
                              |{ c_base_table }~{ c_fields-is_basic_write_dir_ltr }|
                            WHEN zif_sat_c_os_dtel_options=>c_flag-bidi_filtering THEN
                              |{ c_base_table }~{ c_fields-is_bidi_filter_enabled }|
                            WHEN zif_sat_c_os_dtel_options=>c_flag-changedoc_enabled THEN
                              |{ c_base_table }~{ c_fields-is_changedoc_enabled }|
                            WHEN zif_sat_c_os_dtel_options=>c_flag-input_history THEN
                              |{ c_base_table }~{ c_fields-is_input_history_enabled }| ).
  ENDMETHOD.
ENDCLASS.
