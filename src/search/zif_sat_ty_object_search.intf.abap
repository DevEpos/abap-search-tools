"! <p class="shorttext synchronized">Types for Object Search</p>
INTERFACE zif_sat_ty_object_search
  PUBLIC.
  TYPES:
    ty_custom_field TYPE c LENGTH 30,
    "! Row Object Search Result
    BEGIN OF ty_s_search_result,
      object_name     TYPE zsat_entity_id,
      raw_object_name TYPE zsat_entity_id_raw,
      alt_object_name TYPE zsat_entity_id,
      entity_type     TYPE zsat_entity_type,
      sub_object_name TYPE c LENGTH 100,
      sub_object_type TYPE swo_objtyp,
      tadir_type      TYPE trobjtype,
      description     TYPE ddtext,
      devclass        TYPE devclass,
      api_state       TYPE c LENGTH 30,
      custom_field1   TYPE ty_custom_field,
      created_by      TYPE uname,
      created_date    TYPE dats,
      changed_by      TYPE uname,
      changed_date    TYPE dats,
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
      with_api_state           TYPE abap_bool,
      get_all                  TYPE abap_bool,
    END OF ty_s_search_engine_params.

ENDINTERFACE.
