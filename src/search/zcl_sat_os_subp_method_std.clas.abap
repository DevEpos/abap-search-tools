"! <p class="shorttext synchronized">Standard Search provider for Class/Interface Methods</p>
"! Search is implemented in the 'standard' provider way, via single combined
"! SQL command with only a little post processing.
CLASS zcl_sat_os_subp_method_std DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_os_classintf_provider
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS prepare_search     REDEFINITION.
    METHODS determine_grouping REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.
    ALIASES c_method_option            FOR zif_sat_c_object_search~c_method_search_option.

    CONSTANTS:
      BEGIN OF c_alias_names,
        flags       TYPE string VALUE 'flag',
        text        TYPE string VALUE 'text',
        method_text TYPE string VALUE 'meth_text',
        method      TYPE string VALUE 'method',
        param       TYPE string VALUE 'param',
        exception   TYPE string VALUE 'exc',
      END OF c_alias_names.

    CONSTANTS:
      BEGIN OF c_class_fields,
        classname  TYPE string VALUE 'classname',
        package    TYPE string VALUE 'developmentpackage',
        tadir_type TYPE string VALUE 'tadirtype',
      END OF c_class_fields,
      BEGIN OF c_method_fields,
        classname            TYPE string VALUE 'classname',
        methodname           TYPE string VALUE 'methodname',
        methodtype           TYPE string VALUE 'methodtype',
        isabstract           TYPE string VALUE 'isabstract',
        isoptional           TYPE string VALUE 'isoptional',
        isfinal              TYPE string VALUE 'isfinal',
        exposure             TYPE string VALUE 'exposure',
        isusingnewexceptions TYPE string VALUE 'isusingnewexceptions',
        methodlevel          TYPE string VALUE 'methodlevel',
        originalclifname     TYPE string VALUE 'originalclifname',
        originalmethodname   TYPE string VALUE 'originalmethodname',
        createdby            TYPE string VALUE 'createdby',
        createdon            TYPE string VALUE 'createdon',
        changedby            TYPE string VALUE 'changedby',
        changedon            TYPE string VALUE 'changedon',
        category             TYPE string VALUE 'category',
      END OF c_method_fields,
      BEGIN OF c_text_fields,
        language    TYPE string VALUE 'language',
        method      TYPE string VALUE 'component',
        description TYPE string VALUE 'description',
      END OF c_text_fields.

    DATA mv_param_subquery     TYPE string.
    DATA mv_exc_subquery       TYPE string.

    DATA mv_param_filter_count TYPE i.
    DATA mv_exc_filter_count   TYPE i.

    METHODS add_select_fields.
    METHODS configure_method_filters.

    METHODS add_param_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS add_exception_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS configure_search_term_filters.

    METHODS add_flag_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.

    METHODS map_flag_opt_to_field
      IMPORTING
        iv_option     TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sat_os_subp_method_std IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    mv_param_subquery = |SELECT DISTINCT methodname | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_clifmethodparam } | && c_cr_lf &&
                        | WHERE classname = { c_alias_names-method }~{ c_method_fields-classname } | && c_cr_lf &&
                        |   AND methodname = { c_alias_names-method }~{ c_method_fields-methodname } | && c_cr_lf &&
                        |   AND |.

    mv_exc_subquery = |SELECT DISTINCT methodname | && c_cr_lf &&
                        | FROM { zif_sat_c_select_source_id=>zsat_i_clifmethodexception } | && c_cr_lf &&
                        | WHERE classname = { c_alias_names-method }~{ c_method_fields-classname } | && c_cr_lf &&
                        |   AND methodname = { c_alias_names-method }~{ c_method_fields-methodname } | && c_cr_lf &&
                        |   AND |.
  ENDMETHOD.

  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_classinterfacemethod
                           iv_alias  = c_alias_names-method ).

    " join to class/interface always necessary because of devclass/tadir type
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterface }|
                    iv_alias      = c_clif_alias
                    it_conditions = VALUE #( ( field           = c_class_fields-classname
                                               ref_field       = c_method_fields-classname
                                               ref_table_alias = c_alias_names-method
                                               type            = zif_sat_c_join_cond_type=>field
                                               and_or          = zif_sat_c_selection_condition=>and ) ) ).

    " Method Descriptions can not be read via RS_SHORTTEXT_GET
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterfacecomptext }|
                    iv_alias      = c_alias_names-method_text
                    iv_join_type  = zif_sat_c_join_types=>left_outer_join
                    it_conditions = VALUE #( and_or          = zif_sat_c_selection_condition=>and
                                             ref_table_alias = c_alias_names-method
                                             ( field         = c_class_fields-classname
                                               type          = zif_sat_c_join_cond_type=>field
                                               ref_field     = c_method_fields-originalclifname )
                                             ( field         = c_text_fields-method
                                               type          = zif_sat_c_join_cond_type=>field
                                               ref_field     = c_method_fields-originalmethodname )
                                             ( field         = c_text_fields-language
                                               type          = zif_sat_c_join_cond_type=>filter
                                               operator      = zif_sat_c_operator=>equals
                                               tabname_alias = c_alias_names-method_text
                                               value         = sy-langu ) ) ).

    add_select_fields( ).

    add_order_by( iv_fieldname = c_method_fields-classname iv_entity = c_alias_names-method ).
    add_order_by( iv_fieldname = c_method_fields-methodlevel iv_entity = c_alias_names-method if_descending = abap_true ).
    add_order_by( iv_fieldname = c_method_fields-exposure iv_entity = c_alias_names-method if_descending = abap_true ).
    add_order_by( iv_fieldname = c_method_fields-methodname iv_entity = c_alias_names-method ).

    configure_search_term_filters( ).
    configure_class_filters( ).
    configure_method_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD add_select_fields.
    add_select_field( iv_fieldname       = c_method_fields-classname
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-classname
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-methodname
                      iv_fieldname_alias = c_result_fields-method_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-category
                      iv_fieldname_alias = c_result_fields-method_status
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-isabstract
                      iv_fieldname_alias = c_result_fields-method_is_abstract
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-isfinal
                      iv_entity          = c_alias_names-method
                      iv_fieldname_alias = c_result_fields-method_is_final ).
    add_select_field( iv_fieldname       = c_method_fields-exposure
                      iv_entity          = c_alias_names-method
                      iv_fieldname_alias = c_result_fields-method_exposure ).
    add_select_field( iv_fieldname       = c_method_fields-methodlevel
                      iv_entity          = c_alias_names-method
                      iv_fieldname_alias = c_result_fields-method_level ).
    add_select_field( iv_fieldname       = c_method_fields-methodtype
                      iv_entity          = c_alias_names-method
                      iv_fieldname_alias = c_result_fields-method_type ).
    add_select_field( iv_fieldname       = c_method_fields-createdby
                      iv_fieldname_alias = c_result_fields-created_by
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_class_fields-package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_class_fields-tadir_type
                      iv_fieldname_alias = c_result_fields-tadir_type
                      iv_entity          = c_clif_alias ).
    add_select_field( iv_fieldname       = c_text_fields-description
                      " HINT: Method description is written not to 'description' field as default logic
                      "       fills the class/interface description into this field after the sql query has been executed
                      iv_fieldname_alias = c_result_fields-method_descr
                      iv_entity          = c_alias_names-method_text ).
    add_select_field( iv_fieldname       = c_method_fields-createdon
                      iv_fieldname_alias = c_result_fields-created_date
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-changedby
                      iv_fieldname_alias = c_result_fields-changed_by
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-changedon
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_alias_names-method ).
  ENDMETHOD.

  METHOD configure_search_term_filters.
    add_search_terms_to_search(
        iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
        it_field_names = VALUE #( ( |{ c_alias_names-method }~{ c_method_fields-classname }| ) ) ).
    add_search_terms_to_search(
        iv_target      = zif_sat_c_object_search=>c_search_fields-method_name_input_key
        it_field_names = VALUE #( ( |{ c_alias_names-method }~{ c_method_fields-methodname }| ) ) ).
  ENDMETHOD.

  METHOD configure_method_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>)
        WHERE target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.

      CASE <ls_option>-option.
        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = |{ c_alias_names-method }~{ c_method_fields-createdby }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = |{ c_alias_names-method }~{ c_method_fields-changedby }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-description.
          add_option_filter( iv_fieldname = |{ c_alias_names-method_text }~{ mv_description_filter_field }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_method_option-param.
          add_param_filter( <ls_option>-value_range ).

        WHEN c_method_option-exception.
          add_exception_filter( <ls_option>-value_range ).

        WHEN c_method_option-flag.
          add_flag_filter( <ls_option>-value_range ).

        WHEN c_method_option-level.
          add_option_filter( iv_fieldname = |{ c_alias_names-method }~methodlevel|
                             it_values    = <ls_option>-value_range ).

        WHEN c_method_option-visibility.
          add_option_filter( iv_fieldname = |{ c_alias_names-method }~exposure|
                             it_values    = <ls_option>-value_range ).

        WHEN c_method_option-status.
          add_option_filter( iv_fieldname = |{ c_alias_names-method }~category|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-type.
          add_option_filter( iv_fieldname = |{ c_alias_names-method }~methodtype|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = |{ c_alias_names-method }~{ c_method_fields-createdon }|
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = |{ c_alias_names-method }~{ c_method_fields-changedon }|
                           it_values    = <ls_option>-value_range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD determine_grouping.
    CHECK ms_search_engine_params-use_and_cond_for_options = abap_true.

    " Excluding would break the relational division logic and would lead to unreliable results
    IF NOT ( mv_param_filter_count > 1 OR mv_exc_filter_count > 1 ).
      RETURN.
    ENDIF.

    " Create grouping clause
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-classname }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-classname }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-methodname }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-category }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-isabstract }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-isfinal }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-exposure }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-methodlevel }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-methodtype }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-createdby }| ).
    add_group_by_clause( |{ c_clif_alias }~{ c_class_fields-package }| ).
    add_group_by_clause( |{ c_clif_alias }~{ c_class_fields-tadir_type }| ).
    add_group_by_clause( |{ c_alias_names-method_text }~{ c_text_fields-description }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-createdon }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-changedby }| ).
    add_group_by_clause( |{ c_alias_names-method }~{ c_method_fields-changedon }| ).

    IF mv_param_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-param }~parametername| iv_counter_compare = mv_param_filter_count ).
    ENDIF.
    IF mv_exc_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-exception }~exceptionname| iv_counter_compare = mv_exc_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD add_param_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).
    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = 'parametername'
                            iv_fieldname          = |{ c_alias_names-method }~{ c_method_fields-methodname }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_param_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_clifmethodparam }|
                      iv_alias      = c_alias_names-param
                      it_conditions = VALUE #( ref_table_alias = c_alias_names-method
                                               type            = zif_sat_c_join_cond_type=>field
                                               ( field     = c_method_fields-classname
                                                 ref_field = c_method_fields-originalclifname )
                                               ( field     = 'methodname'
                                                 ref_field = c_method_fields-originalmethodname ) ) ).
      add_option_filter( iv_fieldname = |{ c_alias_names-param }~{ 'parametername' }|
                         it_values    = lt_including ).
      mv_param_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.

  METHOD add_exception_filter.
    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding) ).
    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter( iv_subquery_fieldname = 'exceptionname'
                            iv_fieldname          = |{ c_alias_names-method }~{ c_method_fields-methodname }|
                            it_excluding          = lt_excluding
                            iv_subquery           = mv_param_subquery ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_clifmethodexception }|
                      iv_alias      = c_alias_names-exception
                      it_conditions = VALUE #( ref_table_alias = c_alias_names-method
                                               type            = zif_sat_c_join_cond_type=>field
                                               ( field     = c_method_fields-classname
                                                 ref_field = c_method_fields-originalclifname )
                                               ( field     = 'methodname'
                                                 ref_field = c_method_fields-originalmethodname ) ) ).
      add_option_filter( iv_fieldname = |{ c_alias_names-exception }~{ 'exceptionname' }|
                         it_values    = lt_including ).
      mv_exc_filter_count = lines( lt_including ).
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
    result = |{ c_alias_names-method }~| &&
             SWITCH string( iv_option
                            WHEN zif_sat_c_object_search=>c_method_flags-abstract         THEN 'isabstract'
                            WHEN zif_sat_c_object_search=>c_method_flags-optional         THEN 'isoptional'
                            WHEN zif_sat_c_object_search=>c_method_flags-final            THEN 'isfinal'
                            WHEN zif_sat_c_object_search=>c_method_flags-class_exceptions THEN 'isusingnewexceptions' ).
  ENDMETHOD.
ENDCLASS.