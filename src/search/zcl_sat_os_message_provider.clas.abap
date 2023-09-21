"! <p class="shorttext synchronized">Search provider messages in message classes</p>
CLASS zcl_sat_os_message_provider DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_search_provider
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS prepare_search REDEFINITION.

  PRIVATE SECTION.
    ALIASES c_msg_search_params FOR zif_sat_c_object_search~c_message_search_params.

    CONSTANTS:
      BEGIN OF c_aliases,
        msg_clas TYPE string VALUE 'msgclas',
        message  TYPE string VALUE 'msg',
      END OF c_aliases,

      BEGIN OF c_fields,
        message_class       TYPE string VALUE 'messageclass',
        message_number      TYPE string VALUE 'messagenumber',
        short_text          TYPE string VALUE 'shorttext',
        language            TYPE string VALUE 'language',
        created_by          TYPE string VALUE 'createdby',
        created_on          TYPE string VALUE 'createdon',
        changed_by          TYPE string VALUE 'changedby',
        changed_on          TYPE string VALUE 'changedon',
        description         TYPE string VALUE 'description',
        development_package TYPE string VALUE 'developmentpackage',
        is_self_explanatory TYPE string VALUE 'isselfexplanatory',
      END OF c_fields.

    METHODS configure_msg_clas_filters.
    METHODS configure_message_filters.
ENDCLASS.


CLASS zcl_sat_os_message_provider IMPLEMENTATION.
  METHOD prepare_search.
    set_base_select_table( iv_entity = |{ zif_sat_c_select_source_id=>zsat_i_messageclass }|
                           iv_alias  = c_aliases-msg_clas ).

    add_join_table( iv_join_table = |{ zif_sat_c_select_source_id=>zsat_i_message }|
                    iv_alias      = c_aliases-message
                    it_conditions = VALUE #( and_or = zif_sat_c_selection_condition=>and
                                             ( type            = zif_sat_c_join_cond_type=>field
                                               field           = c_fields-message_class
                                               ref_field       = c_fields-message_class
                                               ref_table_alias = c_aliases-msg_clas )
                                             ( field           = c_fields-language
                                               tabname_alias   = c_aliases-message
                                               value           = sy-langu
                                               operator        = zif_sat_c_operator=>equals
                                               type            = zif_sat_c_join_cond_type=>filter ) ) ).

    add_select_field( iv_fieldname       = c_fields-message_class
                      iv_fieldname_alias = c_result_fields-object_name
                      iv_entity          = c_aliases-msg_clas ).
    add_select_field( iv_fieldname       = c_fields-message_class
                      iv_fieldname_alias = c_result_fields-raw_object_name
                      iv_entity          = c_aliases-msg_clas ).
    add_select_field( iv_fieldname = c_fields-changed_by iv_fieldname_alias = c_result_fields-changed_by iv_entity = c_aliases-message ).
    add_select_field( iv_fieldname       = c_fields-changed_on
                      iv_fieldname_alias = c_result_fields-changed_date
                      iv_entity          = c_aliases-message ).
    add_select_field( iv_fieldname = c_fields-description  iv_entity = c_aliases-msg_clas ).
    add_select_field( iv_fieldname       = c_fields-development_package
                      iv_fieldname_alias = c_result_fields-devclass
                      iv_entity          = c_aliases-msg_clas ).
    add_select_field( iv_fieldname       = c_fields-message_number
                      iv_fieldname_alias = c_result_fields-message_number
                      iv_entity          = c_aliases-message ).
    add_select_field( iv_fieldname       = c_fields-short_text
                      iv_fieldname_alias = c_result_fields-message_short_text
                      iv_entity          = c_aliases-message ).
    add_select_field( iv_fieldname = |'{ zif_sat_c_tadir_types=>message_class }'| iv_fieldname_alias = c_result_fields-tadir_type ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                                it_field_names = VALUE #( ( |{ c_aliases-msg_clas }~{ c_fields-message_class }| ) ) ).

    add_search_terms_to_search( iv_target      = zif_sat_c_object_search=>c_search_fields-message_text_input_key
                                it_field_names = VALUE #( ( |{ c_aliases-message }~{ c_fields-short_text }| ) ) ).

    add_order_by( iv_fieldname = c_fields-message_class
                  iv_entity    = c_aliases-msg_clas ).
    add_order_by( iv_fieldname = c_fields-message_number
                  iv_entity    = c_aliases-message ).

    configure_msg_clas_filters( ).
    configure_message_filters( ).

    new_and_cond_list( ).
  ENDMETHOD.

  METHOD configure_message_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>)
            WHERE target = zif_sat_c_object_search=>c_search_fields-message_filter_input_key.

      CASE <ls_option>-option.

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = |{ c_aliases-message }~{ c_fields-changed_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = |{ c_aliases-message }~{ c_fields-changed_on }|
                           it_values    = <ls_option>-value_range ).

        WHEN c_msg_search_params-self_explanatory.
          " '3' - self explanatory
          " ''  - object required documentation
          DATA(lv_self_expl) = COND doku_selfd( WHEN <ls_option>-value_range[ 1 ]-low = abap_true THEN '3' ELSE space  ).
          add_option_filter( iv_fieldname = |{ c_aliases-message }~{ c_fields-is_self_explanatory }|
                             it_values    = VALUE #( ( sign = 'I' option = 'EQ' low = lv_self_expl ) ) ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD configure_msg_clas_filters.
    LOOP AT mo_search_query->mt_search_options ASSIGNING FIELD-SYMBOL(<ls_option>)
            WHERE target = zif_sat_c_object_search=>c_search_fields-object_filter_input_key.

      CASE <ls_option>-option.

        WHEN c_general_search_options-description.
          add_option_filter( iv_fieldname = |{ c_aliases-msg_clas }~{ mv_description_filter_field }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-user.
          add_option_filter( iv_fieldname = |{ c_aliases-msg_clas }~{ c_fields-created_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-created_on.
          add_date_filter( iv_fieldname = |{ c_aliases-msg_clas }~{ c_fields-created_on }|
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_by.
          add_option_filter( iv_fieldname = |{ c_aliases-msg_clas }~{ c_fields-changed_by }|
                             it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-changed_on.
          add_date_filter( iv_fieldname = |{ c_aliases-msg_clas }~{ c_fields-changed_on }|
                           it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-package.
          add_package_filter( iv_fieldname = |{ c_aliases-msg_clas }~{ c_fields-development_package }|
                              it_values    = <ls_option>-value_range ).

        WHEN c_general_search_options-application_component.
          add_appl_comp_filter( it_values          = <ls_option>-value_range
                                iv_ref_field       = CONV #( c_fields-development_package )
                                iv_ref_table_alias = c_aliases-msg_clas ).

        WHEN c_general_search_options-software_component.
          add_softw_comp_filter( it_values          = <ls_option>-value_range
                                 iv_ref_field       = CONV #( c_fields-development_package )
                                 iv_ref_table_alias = c_aliases-msg_clas ).

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
