"! <p class="shorttext synchronized">Types for Object Search</p>
INTERFACE zif_sat_ty_object_search
  PUBLIC.
  TYPES:
    ty_custom_field_short  TYPE c LENGTH 5,
    ty_custom_field_medium TYPE c LENGTH 30,
    ty_custom_field_long   TYPE c LENGTH 100,
    ty_class_subcomp_range TYPE RANGE OF seosconame,

    "! Row Object Search Result
    BEGIN OF ty_s_search_result,
      object_name        TYPE zsat_entity_id,
      raw_object_name    TYPE zsat_entity_id_raw,
      alt_object_name    TYPE zsat_entity_id,
      entity_type        TYPE zsat_entity_type,
      tadir_type         TYPE trobjtype,
      description        TYPE ddtext,
      devclass           TYPE devclass,
      cds_source_type    TYPE c LENGTH 1,
      message_number     TYPE t100-msgnr,
      message_short_text TYPE t100-text,
      method_type        TYPE seomtdtype,
      "! Category (1=Standard,2=Implemented,3=Redefined)
      method_status      TYPE c LENGTH 1,
      method_name        TYPE seocpdname,
      "! Declaring Class/Interface Name of method
      method_decl_clif   TYPE classname,
      "! Method name in original Class/Interface
      method_decl_method TYPE seocmpname,
      method_is_abstract TYPE abap_bool,
      method_is_final    TYPE abap_bool,
      method_exposure    TYPE seoexpose,
      method_level       TYPE seomtddecl,
      method_descr       TYPE ddtext,
      created_by         TYPE uname,
      created_date       TYPE dats,
      changed_by         TYPE uname,
      changed_date       TYPE dats,
    END OF ty_s_search_result,

    "! Table of object search results
    ty_t_search_result TYPE STANDARD TABLE OF ty_s_search_result WITH EMPTY KEY,

    "! <p class="shorttext synchronized">Type for the object search</p>
    ty_search_type     TYPE c LENGTH 15,
    "! <p class="shorttext synchronized">Value range for search option value entry</p>
    BEGIN OF ty_s_value_range,
      sign    TYPE ddsign,
      sign2   TYPE ddsign,
      option  TYPE ddoption,
      option2 TYPE ddoption,
      low     TYPE string,
      high    TYPE string,
    END OF ty_s_value_range,
    "! <p class="shorttext synchronized">Table of option value ranges</p>
    ty_t_value_range TYPE STANDARD TABLE OF ty_s_value_range WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Represents search option with its values</p>
    BEGIN OF ty_s_search_option,
      option      TYPE string,
      "! The target of the option/filter
      target      TYPE string,
      value_range TYPE ty_t_value_range,
    END OF ty_s_search_option,
    "! <p class="shorttext synchronized">Table of search options</p>
    ty_t_search_option TYPE STANDARD TABLE OF ty_s_search_option WITH KEY option,

    "! <p class="shorttext synchronized">Search engine parameters</p>
    BEGIN OF ty_s_search_engine_params,
      use_and_cond_for_options TYPE abap_bool,
      get_all                  TYPE abap_bool,
      custom_options           TYPE zif_sat_ty_adt_types=>ty_t_property,
    END OF ty_s_search_engine_params,

    ty_t_options TYPE RANGE OF string,

    BEGIN OF ty_s_search_term,
      target         TYPE string,
      case_sensitive TYPE string,
      values         TYPE RANGE OF string,
    END OF ty_s_search_term,

    ty_t_search_term TYPE STANDARD TABLE OF ty_s_search_term WITH EMPTY KEY,

    BEGIN OF ty_image_info,
      img_key      TYPE string,
      img_registry TYPE string,
      img_encoded  TYPE string,
    END OF ty_image_info,

    ty_image_infos TYPE STANDARD TABLE OF ty_image_info WITH EMPTY KEY,

    BEGIN OF ty_content_proposal,
      name        TYPE string,
      data        TYPE string,
      description TYPE string,
    END OF ty_content_proposal,

    ty_content_proposals TYPE STANDARD TABLE OF ty_content_proposal WITH EMPTY KEY,

    BEGIN OF ty_option_content_assist,
      assist_type             TYPE string,
      caching                 TYPE abap_bool,
      adt_object_types        TYPE string_table,
      category_term           TYPE string,
      secondary_category_term TYPE string,
      category_scheme         TYPE string,
      proposal_image_source   TYPE string,
      proposal_image_registry TYPE string,
      proposal_values         TYPE ty_content_proposals,
      proposal_images         TYPE ty_image_infos,
    END OF ty_option_content_assist,

    "! <p class="shorttext synchronized">Setting for search option</p>
    BEGIN OF ty_query_filter,
      name             TYPE string,
      data_type        TYPE string,
      allowed_length   TYPE i,
      no_uppercase     TYPE abap_bool,
      single           TYPE abap_bool,
      patterns         TYPE abap_bool,
      key_value        TYPE abap_bool,
      no_negation      TYPE abap_bool,
      description      TYPE string,
      long_description TYPE string,
      internal         TYPE abap_bool,
      img_info         TYPE ty_image_info,
      content_assist   TYPE ty_option_content_assist,
    END OF ty_query_filter,

    "! <p class="shorttext synchronized">List of option configurations</p>
    ty_query_filters TYPE STANDARD TABLE OF ty_query_filter WITH KEY name,

    "! Input field for a search type
    BEGIN OF ty_input_field,
      name           TYPE string,
      label          TYPE string,
      mixed          TYPE abap_bool,
      case_sensitive TYPE abap_bool,
      filters        TYPE ty_query_filters,
    END OF ty_input_field,

    ty_input_fields TYPE STANDARD TABLE OF ty_input_field WITH KEY name,

    BEGIN OF ty_custom_option_value,
      key   TYPE string,
      value TYPE string,
    END OF ty_custom_option_value,

    ty_custom_option_values TYPE STANDARD TABLE OF ty_custom_option_value WITH EMPTY KEY,

    BEGIN OF ty_custom_option,
      key         TYPE string,
      type        TYPE string,
      label       TYPE string,
      description TYPE string,
      values      TYPE ty_custom_option_values,
    END OF ty_custom_option,

    ty_custom_options TYPE STANDARD TABLE OF ty_custom_option WITH EMPTY KEY,

    BEGIN OF ty_adt_type_img_map,
      adt_type   TYPE string,
      image_info TYPE ty_image_info,
    END OF ty_adt_type_img_map,

    ty_adt_type_img_map_entries TYPE STANDARD TABLE OF ty_adt_type_img_map WITH EMPTY KEY,

    BEGIN OF ty_result_output_config,
      types_for_list           TYPE string_table,
      is_list_output_supported TYPE abap_bool,
      groupings                TYPE string_table,
      adt_type_img_map_entries TYPE ty_adt_type_img_map_entries,
    END OF ty_result_output_config,

    ty_result_output_configs TYPE STANDARD TABLE OF ty_result_output_config WITH EMPTY KEY,

    "! Settings for a given search types
    BEGIN OF ty_search_type_config,
      name           TYPE string,
      label          TYPE string,
      img_info       TYPE ty_image_info,
      inputs         TYPE ty_input_fields,
      custom_options TYPE ty_custom_options,
      output_config  TYPE ty_result_output_config,
    END OF ty_search_type_config,

    ty_search_type_configs TYPE STANDARD TABLE OF ty_search_type_config WITH KEY name,

    "! Configuration for all available search types
    BEGIN OF ty_search_config,
      search_types TYPE ty_search_type_configs,
    END OF ty_search_config.

ENDINTERFACE.
