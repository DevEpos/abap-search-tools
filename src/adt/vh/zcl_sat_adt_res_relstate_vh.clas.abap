"! <p class="shorttext synchronized">Release States VH</p>
CLASS zcl_sat_adt_res_relstate_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    CONSTANTS c_split_marker TYPE string VALUE '@@##@@' ##NO_TEXT.
    CONSTANTS c_type TYPE string VALUE 'type' ##NO_TEXT.
    CONSTANTS c_longtext TYPE string VALUE 'longtext' ##NO_TEXT.

    METHODS get_named_items REDEFINITION.

    METHODS get_release_states
      IMPORTING
        i_api_filter_values TYPE zif_sat_ty_adt_types=>ty_r_api_adt_filter_values
      RETURNING
        VALUE(r_result)     TYPE if_adt_named_item=>ty_named_item_list.

    METHODS get_filter_values
      IMPORTING
        i_object_type   TYPE string
      RETURNING
        VALUE(r_result) TYPE zif_sat_ty_adt_types=>ty_r_api_adt_filter_values.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_relstate_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA type_list TYPE string_table.
    DATA filter_values TYPE zif_sat_ty_adt_types=>ty_r_api_adt_filter_values.

    IF p_filter_name IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT p_filter_name AT ',' INTO TABLE type_list.

    LOOP AT type_list INTO DATA(type).
      filter_values = VALUE #( BASE filter_values ( LINES OF get_filter_values( type ) ) ).
    ENDLOOP.

    p_named_item_list = get_release_states( filter_values ).
    p_filter_already_applied = abap_true.
  ENDMETHOD.

  METHOD get_release_states.
    DATA(release_states) = cl_ris_adt_res_release_states=>get_all( ).

    LOOP AT release_states REFERENCE INTO DATA(rel_state).
      IF rel_state->name NOT IN i_api_filter_values.
        CONTINUE.
      ENDIF.

      r_result-items = VALUE #(
          BASE r_result-items
          ( name        = rel_state->name
            description = rel_state->description
            data        = COND string( WHEN rel_state->longtext IS NOT INITIAL
                                       THEN |{ c_type }={ rel_state->type }{ c_split_marker }{ c_longtext }={ rel_state->longtext }|
                                       ELSE rel_state->type ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_filter_values.
    r_result = zcl_sat_api_state_util=>get_instance( )->get_filter_values_for_otype( i_object_type ).
  ENDMETHOD.
ENDCLASS.
