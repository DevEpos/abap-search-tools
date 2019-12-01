"! <p class="shorttext synchronized" lang="en">Search provider for ABAP OO Classes/interfaces</p>
CLASS zcl_sat_os_classintf_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS prepare_search
        REDEFINITION.
    METHODS do_after_search
        REDEFINITION.
    METHODS determine_grouping
        REDEFINITION.
  PRIVATE SECTION.
    ALIASES:
       c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.
    CONSTANTS:
      BEGIN OF c_alias_names,
        base      TYPE string VALUE 'base',
        flags     TYPE string VALUE 'flag',
        text      TYPE string VALUE 'text',
        attribute TYPE string VALUE 'attribute',
        method    TYPE string VALUE 'method',
        interface TYPE string VALUE 'interface',
        friend    TYPE string VALUE 'friend',
        super     TYPE string VALUE 'super',
        api       TYPE string VALUE 'api',
      END OF c_alias_names.

    CONSTANTS:
      BEGIN OF c_fields,
        classintf       TYPE string VALUE 'classname',
        classintf_db    TYPE string VALUE 'clsname',
        flag            TYPE string VALUE 'flag',
        attribute       TYPE string VALUE 'attributename',
        value           TYPE string VALUE 'value',
        method          TYPE string VALUE 'methodname',
        author          TYPE string VALUE 'createdby',
        package         TYPE string VALUE 'developmentpackage',
        language        TYPE string VALUE 'language',
        abap_version    TYPE string VALUE 'abapversion',
        tadir_type      TYPE string VALUE 'tadirtype',
        using_interface TYPE string VALUE 'usedinterface',
        super_class     TYPE string VALUE 'superclass',
        friend          TYPE string VALUE 'friend',
        category        TYPE string VALUE 'category',
      END OF c_fields.

    DATA mv_flag_filter_count TYPE i.
    DATA mv_flag_subquery TYPE string.
    DATA mv_attr_filter_count TYPE i.
    DATA mv_attribute_subquery TYPE string.
    DATA mv_intf_filter_count TYPE i.
    DATA mv_intf_subquery TYPE string.
    DATA mv_meth_filter_count TYPE i.
    DATA mv_meth_subquery TYPE string.
    DATA mv_friend_filter_count TYPE i.
    DATA mv_friend_subquery TYPE string.

    METHODS add_multi_value_filter
      IMPORTING
        it_values                 TYPE zif_sat_ty_object_search=>ty_t_value_range
        iv_join_table             TYPE tabname
        iv_join_table_alias       TYPE string
        iv_filter_field           TYPE string
        iv_subquery               TYPE string
      CHANGING
        cv_including_filter_count TYPE i.

    "! <p class="shorttext synchronized" lang="en">Create filter for ATTR option</p>
    METHODS add_attribute_filter
      IMPORTING
        it_values TYPE zif_sat_ty_object_search=>ty_t_value_range.
ENDCLASS.



CLASS zcl_sat_os_classintf_provider IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
*.. Create sub queries for parameters where boolean operation AND is senseful
    mv_flag_subquery = |SELECT DISTINCT { c_fields-classintf } | && c_cr_lf &&
                       | FROM { zif_sat_c_select_source_id=>zsat_i_classflags } | && c_cr_lf &&
                       | WHERE |.
    mv_attribute_subquery = |SELECT DISTINCT { c_fields-classintf } | && c_cr_lf &&
                            | FROM { zif_sat_c_select_source_id=>zsat_i_classattribute } | && c_cr_lf &&
                            | WHERE |.
    mv_meth_subquery = |SELECT DISTINCT { c_fields-classintf } | && c_cr_lf &&
                       | FROM { zif_sat_c_select_source_id=>zsat_i_classmethod } | && c_cr_lf &&
                       | WHERE |.
    mv_friend_subquery = |SELECT DISTINCT { c_fields-classintf } | && c_cr_lf &&
                         | FROM { zif_sat_c_select_source_id=>zsat_i_globalfriend } | && c_cr_lf &&
                         | WHERE |.
    mv_intf_subquery = |SELECT DISTINCT { c_fields-classintf } | && c_cr_lf &&
                       | FROM { zif_sat_c_select_source_id=>zsat_i_interfaceusage } | && c_cr_lf &&
                       | WHERE |.

  ENDMETHOD.

  METHOD prepare_search.
    DATA: lf_has_type_option TYPE abap_bool,
          lv_base_table      TYPE string.

    set_base_select_table(
        iv_entity     = zif_sat_c_select_source_id=>zsat_i_classinterface
        iv_alias      = c_alias_names-base
    ).

    IF mo_search_query->has_search_terms( ).
      add_search_terms_to_search( VALUE #( ( |{ c_alias_names-base }~{ c_fields-classintf }| ) ) ).
    ENDIF.

    add_select_field( iv_fieldname = c_fields-classintf iv_fieldname_alias = 'entity_id' iv_entity = c_alias_names-base ).
    add_select_field( iv_fieldname = c_fields-classintf iv_fieldname_alias = 'entity_id_raw' iv_entity = c_alias_names-base ).
    add_select_field( iv_fieldname = c_fields-author iv_fieldname_alias = 'created_by' iv_entity = c_alias_names-base ).
    add_select_field( iv_fieldname = c_fields-package  iv_fieldname_alias = 'devclass' iv_entity = c_alias_names-base ).
    add_select_field( iv_fieldname = c_fields-tadir_type iv_fieldname_alias = 'tadir_type' iv_entity = c_alias_names-base ).

    add_order_by( iv_fieldname = c_fields-classintf iv_entity = c_alias_names-base  ).

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

*.......... Find objects via its description
        WHEN c_general_search_options-description.
          add_join_table(
              iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterfacet }|
              iv_alias      = c_alias_names-text
              it_conditions = VALUE #(
                ( field = c_fields-classintf ref_field = c_fields-classintf ref_table_alias = c_alias_names-base type = zif_sat_c_join_cond_type=>field  )
                ( field = c_fields-language tabname_alias = c_alias_names-text value = sy-langu type = zif_sat_c_join_cond_type=>filter  )
              )
          ).
          add_option_filter(
            iv_fieldname = |{ c_alias_names-text }~{ mv_description_filter_field }|
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects with a certain responsible person
        WHEN c_general_search_options-user.
          add_option_filter(
            iv_fieldname = |{ c_alias_names-base }~{ c_fields-author }|
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects which exist in a certain development package
        WHEN c_general_search_options-package.
          add_option_filter(
            iv_fieldname = |{ c_alias_names-base }~{ c_fields-package }|
            it_values    = <ls_option>-value_range
          ).

*.......... Find only objects with a certain type
        WHEN c_general_search_options-type.
          add_option_filter(
              iv_fieldname    = |{ c_alias_names-base }~{ c_fields-tadir_type }|
              it_values       = <ls_option>-value_range
          ).

*.......... Find only objects with a certain class category
        WHEN c_class_intf_search_option-category.
          add_option_filter(
              iv_fieldname    = |{ c_alias_names-base }~{ c_fields-category }|
              it_values       = <ls_option>-value_range
          ).

*.......... Find classes/interfaces according to the given api release status
        WHEN c_general_search_options-release_state.
          add_api_option_filter(
              it_values          = <ls_option>-value_range
              iv_ref_field       = |{ c_fields-classintf }|
              iv_ref_table_alias = c_alias_names-base
              it_tadir_type      = VALUE #( ( 'CLAS' ) ( 'INTF' ) )
          ).

*.......... Find classes/interfaces with certain Attributes
        WHEN c_class_intf_search_option-attribute.
          add_attribute_filter( <ls_option>-value_range ).

*.......... Find classes/interfaces with certain Methods
        WHEN c_class_intf_search_option-method.
          add_multi_value_filter(
            EXPORTING it_values                 = <ls_option>-value_range
                      iv_join_table             = |{ zif_sat_c_select_source_id=>zsat_i_classmethod }|
                      iv_join_table_alias       = c_alias_names-method
                      iv_filter_field           = c_fields-method
                      iv_subquery               = mv_meth_subquery
            CHANGING  cv_including_filter_count = mv_meth_filter_count
          ).

*.......... Find classes/interfaces with certain flag options, e.g. 'Is Abstract'
        WHEN c_class_intf_search_option-flag.
          add_multi_value_filter(
            EXPORTING it_values                 = <ls_option>-value_range
                      iv_join_table             = |{ zif_sat_c_select_source_id=>zsat_i_classflags }|
                      iv_join_table_alias       = c_alias_names-flags
                      iv_filter_field           = c_fields-flag
                      iv_subquery               = mv_flag_subquery
            CHANGING  cv_including_filter_count = mv_flag_filter_count
          ).

*.......... Find classes/interfaces that support a certain abap language
        WHEN c_class_intf_search_option-abap_lang.
          add_option_filter(
              iv_fieldname    = |{ c_alias_names-base }~{ c_fields-abap_version }|
              it_values       = <ls_option>-value_range
          ).

*.......... Find classes/interfaces with certain global friend releationships
        WHEN c_class_intf_search_option-friend.
          add_multi_value_filter(
            EXPORTING it_values                 = <ls_option>-value_range
                      iv_join_table             = |{ zif_sat_c_select_source_id=>zsat_i_globalfriend }|
                      iv_join_table_alias       = c_alias_names-friend
                      iv_filter_field           = c_fields-friend
                      iv_subquery               = mv_friend_subquery
            CHANGING  cv_including_filter_count = mv_friend_filter_count
          ).

*.......... Find classes/interfaces that use certain interfaces
        WHEN c_class_intf_search_option-interface.
          add_multi_value_filter(
            EXPORTING it_values                 = <ls_option>-value_range
                      iv_join_table             = |{ zif_sat_c_select_source_id=>zsat_i_interfaceusage }|
                      iv_join_table_alias       = c_alias_names-interface
                      iv_filter_field           = c_fields-using_interface
                      iv_subquery               = mv_intf_subquery
            CHANGING  cv_including_filter_count = mv_intf_filter_count
          ).

*.......... Find clases/interfaces with certain super classes
        WHEN c_class_intf_search_option-super_type.
          add_join_table(
              iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_superclass }|
              iv_alias      = c_alias_names-super
              it_conditions = VALUE #(
                ( field = c_fields-classintf ref_field = c_fields-classintf ref_table_alias = c_alias_names-base type = zif_sat_c_join_cond_type=>field  )
              )
          ).
          add_option_filter(
            iv_fieldname = |{ c_alias_names-super }~{ c_fields-super_class }|
            it_values    = <ls_option>-value_range
          ).
      ENDCASE.
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD determine_grouping.
    CHECK ms_search_engine_params-use_and_cond_for_options = abap_true.

****.. Excluding would break the relational division logic and would lead to unreliable results
****    CHECK mf_excluding_found = abap_false.
    IF NOT ( mv_flag_filter_count > 1 OR
             mv_attr_filter_count > 1 OR
             mv_meth_filter_count > 1 OR
             mv_friend_filter_count > 1 OR
             mv_intf_filter_count > 1 ).
      RETURN.
    ENDIF.

*.. Create grouping clause
    add_group_by_clause( |{ c_alias_names-base }~{ c_fields-classintf }| ).
    add_group_by_clause( |{ c_alias_names-base }~{ c_fields-author }| ).
    add_group_by_clause( |{ c_alias_names-base }~{ c_fields-package }| ).
    add_group_by_clause( |{ c_alias_names-base }~{ c_fields-tadir_type }| ).

    IF mv_flag_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-flags }~{ c_fields-flag }| iv_counter_compare = mv_flag_filter_count ).
    ENDIF.
    IF mv_attr_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-attribute }~{ c_fields-attribute }| iv_counter_compare = mv_attr_filter_count ).
    ENDIF.
    IF mv_meth_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-method }~{ c_fields-method }| iv_counter_compare = mv_meth_filter_count ).
    ENDIF.
    IF mv_intf_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-interface }~{ c_fields-using_interface }| iv_counter_compare = mv_intf_filter_count ).
    ENDIF.
    IF mv_friend_filter_count > 1.
      add_having_clause( iv_field = |{ c_alias_names-friend }~{ c_fields-friend }| iv_counter_compare = mv_friend_filter_count ).
    ENDIF.
  ENDMETHOD.

  METHOD do_after_search.
*.. To get the correct fallback language for the  descriptions not filled
*.. via SQL
    fill_descriptions( ).
  ENDMETHOD.


  METHOD add_multi_value_filter.
    split_including_excluding(
      EXPORTING it_values    = it_values
      IMPORTING et_including = DATA(lt_including)
                et_excluding = DATA(lt_excluding)
    ).
    IF lt_excluding IS NOT INITIAL.
      create_not_in_filter(
          iv_subquery_fieldname = iv_filter_field
          iv_fieldname          = |{ c_alias_names-base }~{ c_fields-classintf }|
          it_excluding          = lt_excluding
          iv_subquery           = iv_subquery
      ).
    ENDIF.

    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = |{ iv_join_table }|
          iv_alias      = iv_join_table_alias
          it_conditions = VALUE #(
            ( field = c_fields-classintf ref_field = c_fields-classintf ref_table_alias = c_alias_names-base type = zif_sat_c_join_cond_type=>field )
          )
      ).
      add_option_filter(
          iv_fieldname = |{ iv_join_table_alias }~{ iv_filter_field }|
          it_values    = lt_including
      ).
      cv_including_filter_count = lines( lt_including ).
    ENDIF.
  ENDMETHOD.


  METHOD add_attribute_filter.
    DATA: lt_or_seltab TYPE zif_sat_ty_global=>ty_t_or_seltab_sql.


    split_including_excluding( EXPORTING it_values    = it_values
                               IMPORTING et_including = DATA(lt_including)
                                         et_excluding = DATA(lt_excluding)
    ).

*.. Create sub query for negated annotation key/value pairs
    IF lt_excluding IS NOT INITIAL.
      LOOP AT lt_excluding ASSIGNING FIELD-SYMBOL(<ls_excluding>).
        DATA(lt_and_seltab) = VALUE zif_sat_ty_global=>ty_t_seltab_sql(
           ( sqlfieldname = c_fields-attribute
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
          iv_fieldname = |{ c_alias_names-base }~{ c_fields-classintf }|
          iv_subquery  = mv_attribute_subquery
      ).
    ENDIF.

*.. Add filters for including annotation key/value pairs
    IF lt_including IS NOT INITIAL.
      add_join_table(
          iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classattribute }|
          iv_alias      = c_alias_names-attribute
          it_conditions = VALUE #(
            ( field = c_fields-classintf ref_field = c_fields-classintf ref_table_alias = c_alias_names-base type = zif_sat_c_join_cond_type=>field )
          )
      ).
      mv_attr_filter_count = lines( lt_including ).
      new_and_cond_list( ).
      LOOP AT lt_including INTO DATA(ls_filter_value).
        add_filter( VALUE #( sqlfieldname = |{ c_alias_names-attribute }~{ c_fields-attribute }|
                             sign         = ls_filter_value-sign
                             option       = ls_filter_value-option
                             low          = ls_filter_value-low )  ).

        IF ls_filter_value-high IS NOT INITIAL.
          add_filter( VALUE #( sqlfieldname = |{ c_alias_names-attribute }~{ c_fields-value }|
                               sign         = ls_filter_value-sign2
                               option       = ls_filter_value-option2
                               low          = ls_filter_value-high )  ).
        ENDIF.
        new_or_cond_list( ).
      ENDLOOP.

      new_and_cond_list( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

