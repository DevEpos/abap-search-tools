"! <p class="shorttext synchronized">Search provider for Dictionary Views</p>
CLASS zcl_sat_os_ddicview_provider DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_sat_base_search_provider.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS prepare_search REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_search_params FOR zif_sat_c_object_search~c_ddicview_search_params.

    CONSTANTS:
      c_base_table  TYPE string VALUE 'base',
      c_field_table TYPE string VALUE 'field',
      BEGIN OF c_fields,
        alias               TYPE string VALUE 'field',
        viewname            TYPE string VALUE 'viewname',
        type                TYPE string VALUE 'viewclass',
        fieldname           TYPE string VALUE 'fieldname',
        description         TYPE string VALUE 'description',
        primary_table       TYPE string VALUE 'primarytable',
        development_package TYPE string VALUE 'developmentpackage',
        delivery_class      TYPE string VALUE 'deliveryclass',
        created_by          TYPE string VALUE 'createdby',
        created_date        TYPE string VALUE 'createddate',
        changed_by          TYPE string VALUE 'changedby',
        changed_date        TYPE string VALUE 'changeddate',
        maintenance_flag    TYPE string VALUE 'maintenanceflag',
      END OF c_fields.

    METHODS configure_filters.
ENDCLASS.


CLASS zcl_sat_os_ddicview_provider IMPLEMENTATION.
  METHOD prepare_search.
    set_base_select_table(
        iv_entity     = zif_sat_c_select_source_id=>zsat_i_ddicview
        iv_alias      = c_base_table
        it_parameters = VALUE #(
            ( param_name = 'p_language' param_value = zcl_sat_system_helper=>get_system_language( ) ) ) ).

    add_select_field( iv_fieldname = c_fields-viewname iv_fieldname_alias = c_result_fields-object_name iv_entity = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-viewname
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname = c_fields-description iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-created_by iv_fieldname_alias = c_result_fields-created_by iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-created_date iv_fieldname_alias = c_result_fields-created_date iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-changed_by iv_fieldname_alias = c_result_fields-changed_by iv_entity = c_base_table ).
    add_select_field( iv_fieldname = c_fields-changed_date iv_fieldname_alias = c_result_fields-changed_date iv_entity = c_base_table ).
    add_select_field( iv_fieldname       = c_fields-development_package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_base_table ).
    add_select_field( iv_fieldname       = |'{ zif_sat_c_entity_type=>view }'|
                      iv_fieldname_alias = c_result_fields-entity_type ).

    add_order_by( iv_fieldname = c_fields-viewname iv_entity = c_base_table  ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_field_names = VALUE #( ( |{ c_base_table }~{ c_fields-viewname }| ) ) ).

    configure_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD configure_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

        WHEN c_general_search_options-description.
          add_option_filter( iv_fieldname = mv_description_filter_field
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = c_fields-created_by
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-package.
          add_option_filter( iv_fieldname = c_fields-development_package
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-software_component.
          add_softw_comp_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-development_package )
                                 iv_ref_table_alias = c_base_table ).

        WHEN c_general_search_options-application_component.
          add_appl_comp_filter( it_values          = <ls_option>-value_range
                                iv_ref_field       = CONV #( c_fields-development_package )
                                iv_ref_table_alias = c_base_table ).

        WHEN c_general_search_options-type.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-type }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_search_params-field.
*          add_field_filter( <ls_option>-value_range ).

        WHEN c_search_params-primary_table.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-primary_table }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_search_params-delivery_class.
          add_option_filter( iv_fieldname = c_fields-delivery_class
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = c_fields-created_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = c_fields-changed_date
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-maintenance.
          add_option_filter( iv_fieldname = |{ c_base_table }~{ c_fields-maintenance_flag }|
                             it_values    = <ls_option>-value_range ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
