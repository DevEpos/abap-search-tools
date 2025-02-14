"! <p class="shorttext synchronized">Mapper of filter values</p>
CLASS zcl_sat_table_filter_mapper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS map_maintflag_to_int
      IMPORTING
        iv_value      TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS map_enhanccat_to_int
      IMPORTING
        iv_value      TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sat_table_filter_mapper IMPLEMENTATION.
  METHOD map_maintflag_to_int.
    result = SWITCH #( iv_value
                       WHEN zif_sat_c_os_dtab_options=>c_table_maintenance-ext-allowed THEN
                         zif_sat_c_os_dtab_options=>c_table_maintenance-int-allowed
                       WHEN zif_sat_c_os_dtab_options=>c_table_maintenance-ext-allowed_with_restr THEN
                         zif_sat_c_os_dtab_options=>c_table_maintenance-int-allowed_with_restr
                       WHEN zif_sat_c_os_dtab_options=>c_table_maintenance-ext-not_allowed THEN
                         zif_sat_c_os_dtab_options=>c_table_maintenance-int-not_allowed ).
  ENDMETHOD.

  METHOD map_enhanccat_to_int.
    result = SWITCH #( iv_value
                       WHEN zif_sat_c_os_tabl_options=>c_tab_enh_categories-ext-char_like THEN
                         zif_sat_c_os_tabl_options=>c_tab_enh_categories-int-char_like
                       WHEN zif_sat_c_os_tabl_options=>c_tab_enh_categories-ext-not_classified THEN
                         zif_sat_c_os_tabl_options=>c_tab_enh_categories-int-not_classified
                       WHEN zif_sat_c_os_tabl_options=>c_tab_enh_categories-ext-char_like_and_numeric THEN
                         zif_sat_c_os_tabl_options=>c_tab_enh_categories-int-char_like_and_numeric
                       WHEN zif_sat_c_os_tabl_options=>c_tab_enh_categories-ext-not_extendable THEN
                         zif_sat_c_os_tabl_options=>c_tab_enh_categories-int-not_extendable
                       WHEN zif_sat_c_os_tabl_options=>c_tab_enh_categories-ext-any THEN
                         zif_sat_c_os_tabl_options=>c_tab_enh_categories-int-any ).
  ENDMETHOD.
ENDCLASS.
