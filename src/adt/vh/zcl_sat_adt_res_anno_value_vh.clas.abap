"! <p class="shorttext synchronized">Resource for reading Annotation values</p>
"! Note: DDLA_RT_HEADER and DDLA_RT_ENUMS tables or only available starting
"!       with NW 7.52
CLASS zcl_sat_adt_res_anno_value_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_ddla_rt_header_tab TYPE tabname VALUE 'DDLA_RT_HEADER'.

    TYPES ty_t_anno_name_range TYPE RANGE OF zsat_i_cdsannotationvalue-annotationnameupper.
    TYPES ty_t_anno_value_range TYPE RANGE OF zsat_i_cdsannotationvalue-value.

    "! <p class="shorttext synchronized">Find boolean annotation values for the given criteria</p>
    METHODS find_boolean_values
      IMPORTING
        it_anno_name_range  TYPE ty_t_anno_name_range
        it_anno_value_range TYPE ty_t_anno_value_range
      CHANGING
        cs_named_items      TYPE if_adt_named_item=>ty_named_item_list
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Find enum values for the given criteria</p>
    "! @parameter it_anno_name_range  | <p class="shorttext synchronized">Range of annotation names</p>
    "! @parameter it_anno_value_range | <p class="shorttext synchronized">Range of annotation values</p>
    "! @parameter iv_max_row_count    | <p class="shorttext synchronized">Maximum number of results for select</p>
    "! @parameter cs_named_items      | <p class="shorttext synchronized">List of found annotation values</p>
    METHODS find_enum_values
      IMPORTING
        it_anno_name_range  TYPE ty_t_anno_name_range
        it_anno_value_range TYPE ty_t_anno_value_range
        iv_max_row_count    TYPE i
      CHANGING
        cs_named_items      TYPE if_adt_named_item=>ty_named_item_list
      RAISING
        cx_adt_rest.
ENDCLASS.


CLASS zcl_sat_adt_res_anno_value_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_anno_value_range TYPE ty_t_anno_value_range.
    DATA lt_anno_name_range TYPE ty_t_anno_name_range.

    IF p_filter_name IS NOT INITIAL.
      lt_anno_value_range = VALUE #( ( sign   = 'I'
                                       option = 'CP'
                                       low    = to_upper( replace( val = p_filter_name sub = '#' with = '##' ) ) ) ).
    ENDIF.

    IF p_filter_data IS NOT INITIAL.
      lt_anno_name_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_data ) ) ).
    ENDIF.

    find_boolean_values( EXPORTING it_anno_name_range  = lt_anno_name_range
                                   it_anno_value_range = lt_anno_value_range
                         CHANGING  cs_named_items      = p_named_item_list ).
    find_enum_values( EXPORTING it_anno_name_range  = lt_anno_name_range
                                it_anno_value_range = lt_anno_value_range
                                iv_max_row_count    = p_filter_max_item_count
                      CHANGING  cs_named_items      = p_named_item_list ).

    SORT p_named_item_list-items.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.

  METHOD find_boolean_values.
    DATA lf_boolean_anno_found TYPE abap_bool.

    SELECT SINGLE @abap_true
      FROM (c_ddla_rt_header_tab)
      WHERE key_upper IN @it_anno_name_range
        AND value_type = 'Boolean'
      INTO @lf_boolean_anno_found.

    IF lf_boolean_anno_found = abap_true.
      IF 'TRUE' IN it_anno_value_range.
        cs_named_items-items = VALUE #( BASE cs_named_items-items ( name = 'true' ) ).
      ENDIF.
      IF 'FALSE' IN it_anno_value_range.
        cs_named_items-items = VALUE #( BASE cs_named_items-items ( name = 'false' ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD find_enum_values.
    DATA lt_from TYPE string_table.

    lt_from = VALUE #( ( |ddla_rt_enums as enum| )
                       ( | INNER JOIN ddla_rt_header AS anno| )
                       ( |   ON anno~key_guid = enum~key_guid| ) ).

    TRY.
        SELECT DISTINCT
               concat( '#',enum~symbol )  AS name
          FROM (lt_from)
          WHERE upper( concat( '#', enum~symbol ) ) IN @it_anno_value_range
            AND key_upper                           IN @it_anno_name_range
          APPENDING CORRESPONDING FIELDS OF TABLE @cs_named_items-items
          UP TO @iv_max_row_count ROWS.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_rest_error
          EXPORTING textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
