"! <p class="shorttext synchronized" lang="en">Resource for reading ABAP Language Values</p>
CLASS zcl_sat_adt_res_class_alang_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_named_items
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_adt_res_class_alang_vh IMPLEMENTATION.

  METHOD get_named_items.
    DATA(lt_abap_languages) = zcl_sat_clif_search_param_util=>get_abap_languages( ).

    LOOP AT lt_abap_languages INTO DATA(lv_category).
      TRY.
          zcl_sat_clif_search_param_util=>convert_abap_lang_to_ext(
            EXPORTING iv_internal    = CONV #( lv_category )
            IMPORTING ev_external    = DATA(lv_external)
                      ev_description = DATA(lv_description)
          ).
        CATCH zcx_sat_conversion_exc.
          CONTINUE.
      ENDTRY.
      p_named_item_list-items = VALUE #( BASE p_named_item_list-items
        ( name = lv_external description = lv_description )
      ).
    ENDLOOP.

    p_filter_already_applied = abap_true.
  ENDMETHOD.

ENDCLASS.
