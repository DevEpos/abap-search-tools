"! <p class="shorttext synchronized">Search provider for Class/Interface Methods</p>
CLASS zcl_sat_os_method_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS prepare_search REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.

    CONSTANTS:
      BEGIN OF c_alias_names,
        class     TYPE string VALUE 'cls',
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
      BEGIN OF c_class_fields,
        classname       TYPE string VALUE 'classname',
        flag            TYPE string VALUE 'flag',
        author          TYPE string VALUE 'createdby',
        created_on      TYPE string VALUE 'createdon',
        changed_on      TYPE string VALUE 'changedon',
        changed_by      TYPE string VALUE 'changedby',
        package         TYPE string VALUE 'developmentpackage',
        language        TYPE string VALUE 'language',
        abap_version    TYPE string VALUE 'abapversion',
        tadir_type      TYPE string VALUE 'tadirtype',
        using_interface TYPE string VALUE 'usedinterface',
        super_class     TYPE string VALUE 'superclass',
        friend          TYPE string VALUE 'friend',
        category        TYPE string VALUE 'category',
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
ENDCLASS.


CLASS zcl_sat_os_method_provider IMPLEMENTATION.
  METHOD prepare_search.
    set_base_select_table( iv_entity = zif_sat_c_select_source_id=>zsat_i_classinterfacemethod
                           iv_alias  = c_alias_names-method ).

    " join to class/interface always necessary because of devclass/tadir type
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterface }|
                    iv_alias      = c_alias_names-class
                    it_conditions = VALUE #( ( field           = c_class_fields-classname
                                               ref_field       = c_method_fields-classname
                                               ref_table_alias = c_alias_names-method
                                               type            = zif_sat_c_join_cond_type=>field
                                               and_or          = zif_sat_c_selection_condition=>and ) ) ).

    " Method Descriptions can not be read via RS_SHORTTEXT_GET
    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_classinterfacecomptext }|
                    iv_alias      = c_alias_names-text
                    iv_join_type  = zif_sat_c_join_types=>left_outer_join
                    it_conditions = VALUE #( type            = zif_sat_c_join_cond_type=>field
                                             and_or          = zif_sat_c_selection_condition=>and
                                             ref_table_alias = c_alias_names-method
                                             ( field     = c_class_fields-classname
                                               ref_field = c_method_fields-originalclifname )
                                             ( field     = c_text_fields-method
                                               ref_field = c_method_fields-originalmethodname ) ) ).

    add_search_terms_to_search(
        iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
        it_field_names = VALUE #( ( |{ c_alias_names-method }~{ c_method_fields-classname }| ) ) ).
    add_search_terms_to_search(
        iv_target      = zif_sat_c_object_search=>c_search_fields-method_name_input_key
        it_field_names = VALUE #( ( |{ c_alias_names-method }~{ c_method_fields-methodname }| ) ) ).

    add_select_field( iv_fieldname       = c_method_fields-classname
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-classname
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-methodname
                      iv_fieldname_alias = c_result_fields-custom_field_long1
                      iv_entity          = c_alias_names-method ).
    add_select_Field( iv_fieldname       = c_method_fields-category
                      iv_fieldname_alias = c_result_fields-custom_field_short1
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-createdby
                      iv_fieldname_alias = c_result_fields-created_by
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_class_fields-package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_alias_names-class ).
    add_select_field( iv_fieldname       = c_class_fields-tadir_type
                      iv_fieldname_alias = c_result_fields-tadir_type
                      iv_entity          = c_alias_names-class ).
    add_select_field( iv_fieldname       = c_text_fields-description
                      iv_fieldname_alias = c_result_fields-description
                      iv_entity          = c_alias_names-text ).
    add_select_field( iv_fieldname       = c_method_fields-createdon
                      iv_fieldname_alias = c_result_fields-created_date
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-changedby
                      iv_fieldname_alias = c_result_fields-changed_by
                      iv_entity          = c_alias_names-method ).
    add_select_field( iv_fieldname       = c_method_fields-changedon
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_alias_names-method ).

    add_order_by( iv_fieldname = c_method_fields-classname iv_entity = c_alias_names-method ).
    add_order_by( iv_fieldname = c_method_fields-methodname iv_entity = c_alias_names-method ).
  ENDMETHOD.
ENDCLASS.
