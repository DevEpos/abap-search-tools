"! <p class="shorttext synchronized" lang="en">Search provider for ABAP OO Classes/interfaces</p>
CLASS zcl_sat_os_classintf_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS prepare_search
        REDEFINITION.
    METHODS do_after_search
        REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      c_base_table TYPE string VALUE 'base',
      c_text_alias TYPE string VALUE 'text'.
ENDCLASS.



CLASS zcl_sat_os_classintf_provider IMPLEMENTATION.

  METHOD prepare_search.
    " Table for classes/interfaces: seoclass (simple) / seoclassdf (definition with more properties)
    " Text Table                  : seoclasstx
    " Friend Table                : seofriends
    " Relationships (interfaces,
    "    inheritence, ...)        : seometarel - Fieldinfo: (reltype(1) = interface, reltype(2) = inheritence, refclsname = (using interface / super class)
    " Method/Attribute            : seocompodf (different columns for getting information about method/attribute type)

    DATA: lf_has_type_option TYPE abap_bool,
          lv_base_table      TYPE string.


    set_base_select_table(
        iv_entity     = zif_sat_c_select_source_id=>zsat_i_classinterface
        iv_alias      = c_base_table
    ).

    IF mo_search_query->has_search_terms( ).
      add_search_terms_to_search( VALUE #( ( |{ c_base_table }~classinterface| ) ) ).
    ENDIF.

    add_select_field( iv_fieldname = 'classinterface' iv_fieldname_alias = 'entity_id' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'classinterface' iv_fieldname_alias = 'entity_id_raw' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'createdby' iv_fieldname_alias = 'created_by' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'developmentpackage' iv_fieldname_alias = 'devclass' iv_entity = c_base_table ).
    add_select_field( iv_fieldname = 'tadirtype' iv_fieldname_alias = 'tadir_type' iv_entity = c_base_table ).

    add_order_by( iv_fieldname = 'classinterface' iv_entity = c_base_table  ).

    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>).

      CASE <ls_option>-option.

*.......... Find objects via its description
        WHEN zif_sat_c_object_search=>c_search_option-by_description.
          add_join_table(
              iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterfacet }|
              iv_alias      = c_text_alias
              it_conditions = VALUE #(
                ( field = 'classinterface' ref_field = 'classinterface' ref_table_alias = c_base_table type = zif_sat_c_join_cond_type=>field  )
                ( field = 'language' tabname_alias = c_text_alias value = sy-langu type = zif_sat_c_join_cond_type=>filter  )
              )
          ).
          add_option_filter(
            iv_fieldname = |{ c_text_alias }~{ mv_description_filter_field }|
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects with a certain responsible person
        WHEN zif_sat_c_object_search=>c_search_option-by_owner.
          add_option_filter(
            iv_fieldname = |{ c_base_table }~{ 'createdby' }|
            it_values    = <ls_option>-value_range
          ).

*.......... Find objects which exist in a certain development package
        WHEN zif_sat_c_object_search=>c_search_option-by_package.
          add_option_filter(
            iv_fieldname = |{ c_base_table }~{ 'developmentpackage' }|
            it_values    = <ls_option>-value_range
          ).

*.......... Find only objects with a certain type
        WHEN zif_sat_c_object_search=>c_search_option-by_type.

      ENDCASE.
    ENDLOOP.

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD do_after_search.
*.. To get the correct fallback language for the  descriptions not filled
*.. via SQL
    fill_descriptions( ).
  ENDMETHOD.

ENDCLASS.
