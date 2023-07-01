"! <p class="shorttext synchronized">Types for Object Search</p>
INTERFACE zif_sat_ty_object_search
  PUBLIC.
  TYPES:
    ty_custom_field_short  TYPE c LENGTH 5,
    ty_custom_field_medium TYPE c LENGTH 30,
    ty_custom_field_long   TYPE c LENGTH 100,

    "! Row Object Search Result
    BEGIN OF ty_s_search_result,
      object_name          TYPE zsat_entity_id,
      raw_object_name      TYPE zsat_entity_id_raw,
      alt_object_name      TYPE zsat_entity_id,
      entity_type          TYPE zsat_entity_type,
      sub_object_name      TYPE c LENGTH 100,
      sub_object_type      TYPE swo_objtyp,
      tadir_type           TYPE trobjtype,
      description          TYPE ddtext,
      devclass             TYPE devclass,
      api_state            TYPE c LENGTH 30,
      custom_field_short1  TYPE ty_custom_field_short,
      custom_field_short2  TYPE ty_custom_field_short,
      custom_field_short3  TYPE ty_custom_field_short,
      custom_field_short4  TYPE ty_custom_field_short,
      custom_field_short5  TYPE ty_custom_field_short,
      custom_field_short6  TYPE ty_custom_field_short,
      custom_field_short7  TYPE ty_custom_field_short,
      custom_field_short8  TYPE ty_custom_field_short,
      custom_field_short9  TYPE ty_custom_field_short,
      custom_field_short10 TYPE ty_custom_field_short,
      custom_field_medium1 TYPE ty_custom_field_medium,
      custom_field_medium2 TYPE ty_custom_field_medium,
      custom_field_medium3 TYPE ty_custom_field_medium,
      custom_field_medium4 TYPE ty_custom_field_medium,
      custom_field_medium5 TYPE ty_custom_field_medium,
      custom_field_long1   TYPE ty_custom_field_long,
      custom_field_long2   TYPE ty_custom_field_long,
      custom_field_long3   TYPE ty_custom_field_long,
      custom_field_long4   TYPE ty_custom_field_long,
      custom_field_long5   TYPE ty_custom_field_long,
      created_by           TYPE uname,
      created_date         TYPE dats,
      changed_by           TYPE uname,
      changed_date         TYPE dats,
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

    BEGIN OF ty_s_search_term,
      target TYPE string,
      values TYPE RANGE OF string,
    END OF ty_s_search_term,

    ty_t_search_term TYPE STANDARD TABLE OF ty_s_search_term WITH EMPTY KEY,

    "! <p class="shorttext synchronized">Search engine parameters</p>
    BEGIN OF ty_s_search_engine_params,
      use_and_cond_for_options TYPE abap_bool,
      with_api_state           TYPE abap_bool,
      get_all                  TYPE abap_bool,
    END OF ty_s_search_engine_params,

    ty_t_options TYPE RANGE OF string,

    BEGIN OF ty_s_proposal_image,
      img_key     TYPE string,
      img_encoded TYPE string,
    END OF ty_s_proposal_image,

    ty_t_proposal_image TYPE STANDARD TABLE OF ty_s_proposal_image WITH EMPTY KEY,

    BEGIN OF ty_s_option_content_assist,
      assist_type             TYPE string,
      adt_object_type         TYPE string,
      category_term           TYPE string,
      secondary_category_term TYPE string,
      category_scheme         TYPE string,
      proposal_image_source   TYPE string,
      proposal_images         TYPE ty_t_proposal_image,
    END OF ty_s_option_content_assist,

    "! <p class="shorttext synchronized">Setting for search option</p>
    BEGIN OF ty_s_query_filter,
      name             TYPE string,
      data_type        TYPE string,
      allowed_length   TYPE i,
      no_uppercase     TYPE abap_bool,
      single           TYPE abap_bool,
      patterns         TYPE abap_bool,
      caching          TYPE abap_bool,
      key_value        TYPE abap_bool,
      no_negation      TYPE abap_bool,
      long_description TYPE string,
      img_key          TYPE string,
      img_encoded      TYPE string,
      internal         TYPE abap_bool,
      content_assist   TYPE ty_s_option_content_assist,
    END OF ty_s_query_filter,

    "! <p class="shorttext synchronized">List of option configurations</p>
    ty_t_query_filter TYPE STANDARD TABLE OF ty_s_query_filter WITH KEY name,

    "! Input field for a search type
    BEGIN OF ty_s_input_field,
      name    TYPE string,
      label   TYPE string,
      mixed   TYPE abap_bool,
      filters TYPE ty_t_query_filter,
    END OF ty_s_input_field,

    ty_t_input_field TYPE STANDARD TABLE OF ty_s_input_field WITH KEY name,

    "! Settings for a given search types
    BEGIN OF ty_s_search_type,
      name   TYPE string,
      label  TYPE string,
      inputs TYPE ty_t_input_field,
    END OF ty_s_search_type,

    ty_t_search_type TYPE STANDARD TABLE OF ty_s_search_type WITH KEY name,

    "! Configuration for all available search types
    BEGIN OF ty_s_search_config,
      search_types TYPE ty_t_search_type,
    END OF ty_s_search_config.

ENDINTERFACE.
