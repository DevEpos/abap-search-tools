"! <p class="shorttext synchronized">Filter definitions for db table / view</p>
CLASS zcl_sat_table_filter_values DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_maintenance_filt_values
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_content_proposals.

    CLASS-METHODS get_enhancecat_filt_values
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_content_proposals.
ENDCLASS.


CLASS zcl_sat_table_filter_values IMPLEMENTATION.
  METHOD get_maintenance_filt_values.
    result = VALUE #( ( name        = zif_sat_c_object_search=>c_table_maintenance-ext-allowed
                        description = 'Display/Maintenance Allowed' )
                      ( name        = zif_sat_c_object_search=>c_table_maintenance-ext-allowed_with_restr
                        description = 'Display/Maintenance Allowed with Restrictions' )
                      ( name        = zif_sat_c_object_search=>c_table_maintenance-ext-not_allowed
                        description = 'Display/Maintenance Not Allowed' ) ).
  ENDMETHOD.

  METHOD get_enhancecat_filt_values.
    result = VALUE #( ( name        = zif_sat_c_object_search=>c_tab_enh_categories-ext-not_classified
                        description = 'Not Classified' )
                      ( name        = zif_sat_c_object_search=>c_tab_enh_categories-ext-not_extendable
                        description = 'Cannot be Enhanced' )
                      ( name        = zif_sat_c_object_search=>c_tab_enh_categories-ext-char_like
                        description = 'Can be enhanced - character-like' )
                      ( name        = zif_sat_c_object_search=>c_tab_enh_categories-ext-char_like_and_numeric
                        description = 'Can be enhanced - character-like or numeric' )
                      ( name        = zif_sat_c_object_search=>c_tab_enh_categories-ext-any
                        description = 'Can be enhanced (deep)' ) ).
  ENDMETHOD.
ENDCLASS.
