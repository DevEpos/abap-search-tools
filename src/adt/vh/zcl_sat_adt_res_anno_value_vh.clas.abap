"! <p class="shorttext synchronized" lang="en">Resource for reading Annotation values</p>
CLASS zcl_sat_adt_res_anno_value_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_named_items
        REDEFINITION.
  PRIVATE SECTION.
    TYPES ty_t_anno_name_range TYPE RANGE OF zsat_i_cdsannotationvalue-annotationnameupper.
    TYPES ty_t_anno_value_range TYPE RANGE OF zsat_i_cdsannotationvalue-value.

    "! <p class="shorttext synchronized" lang="en">Find enum values for the given criteria</p>
    "! Only available starting from NW Release <strong>7.52</strong>
    "! @parameter it_anno_name_range | <p class="shorttext synchronized" lang="en">Range of annotation names</p>
    "! @parameter it_anno_value_range | <p class="shorttext synchronized" lang="en">Range of annotation values</p>
    "! @parameter iv_max_row_count | <p class="shorttext synchronized" lang="en">Maximum number of results for select</p>
    "! @parameter cs_named_items | <p class="shorttext synchronized" lang="en">List of found annotation values</p>
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
    DATA: lt_anno_ddlname_range TYPE RANGE OF ddlaname,
          lt_anno_value_range   TYPE ty_t_anno_value_range,
          lt_anno_name_range    TYPE ty_t_anno_name_range.

    lt_anno_ddlname_range = VALUE #(
      ( sign = 'I' option = 'EQ' low = 'ABAPANNOTATION' )
      ( sign = 'I' option = 'EQ' low = 'COMPATIBILITYCONTRACT' )
      ( sign = 'I' option = 'CP' low = 'DEMO*' )
      ( sign = 'I' option = 'EQ' low = 'FEATURE' )
      ( sign = 'I' option = 'CP' low = 'KTD*' )
      ( sign = 'I' option = 'EQ' low = 'LANGUAGEDEPENDENCY' )
      ( sign = 'I' option = 'EQ' low = 'MAPPINGROLE' )
    ).

    IF p_filter_name IS NOT INITIAL.
      lt_anno_value_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( replace( val = p_filter_name sub = '#' with = '##' ) ) ) ).
    ENDIF.

    IF p_filter_data IS NOT INITIAL.
      lt_anno_name_range =  VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_data ) ) ).
    ENDIF.

    SELECT
      FROM zsat_i_cdsannotationvalue
      FIELDS DISTINCT
             value AS name
      WHERE annotationnameupper IN @lt_anno_name_range
        AND upper( value )      IN @lt_anno_value_range
      ORDER BY value
    INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    find_enum_values( EXPORTING it_anno_name_range  = lt_anno_name_range
                                it_anno_value_range = lt_anno_value_range
                                iv_max_row_count    = p_filter_max_item_count
                      CHANGING  cs_named_items      = p_named_item_list ).

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.


  METHOD find_enum_values.
    DATA: lt_select TYPE string_table,
          lt_from   TYPE string_table.

    lt_from = VALUE #(
      ( |ddla_rt_header AS anno| )
      ( | INNER JOIN ddla_rt_enums as enum| )
      ( |   ON enum~key_guid = anno~key_guid| )
    ).

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
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_error ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
