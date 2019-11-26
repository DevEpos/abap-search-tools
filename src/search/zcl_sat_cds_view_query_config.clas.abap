"! <p class="shorttext synchronized" lang="en">Configuration for CDS View Query</p>
CLASS zcl_sat_cds_view_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type
        REDEFINITION.
    METHODS zif_sat_object_search_config~map_option
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
       c_cds_options FOR zif_sat_c_object_search~c_cds_search_params.
ENDCLASS.



CLASS zcl_sat_cds_view_query_config IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mt_options = VALUE #(
      ( option = c_general_options-package allowed_length = 30 )
      ( option = c_general_options-user allowed_length = 12 )
      ( option = c_general_options-release_state )
      ( option = c_cds_options-params single = abap_true no_negation = abap_true )
      ( option = c_cds_options-param )
      ( option = c_cds_options-select_from allowed_length = 30 )
      ( option = c_cds_options-association allowed_length = 30 )
      ( option = c_cds_options-annotation key_value = abap_true )
      ( option = c_cds_options-field allowed_length = 30 )
      ( option = c_general_options-type )
      ( option = c_general_options-description allowed_length = 40 )
      ( option = c_cds_options-extended_by allowed_length = 30 no_negation = abap_true )
      ( option = c_general_options-max_rows single = abap_true no_negation = abap_true )
    ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-cds_view.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~map_option.
    cv_option = SWITCH #( cv_option
      WHEN c_search_option-by_package     THEN c_general_options-package
      WHEN c_search_option-by_owner       THEN c_general_options-user
      WHEN c_search_option-by_api         THEN c_general_options-release_state
      WHEN c_search_option-by_params      THEN c_cds_options-params
      WHEN c_search_option-by_param       THEN c_cds_options-param
      WHEN c_search_option-by_select_from THEN c_cds_options-select_from
      WHEN c_search_option-by_association THEN c_cds_options-association
      WHEN c_search_option-by_anno        THEN c_cds_options-annotation
      WHEN c_search_option-by_field       THEN c_cds_options-field
      WHEN c_search_option-by_type        THEN c_general_options-type
      WHEN c_search_option-by_description THEN c_general_options-description
      WHEN c_search_option-by_extensions  THEN c_cds_options-extended_by
      WHEN c_search_option-max_rows       THEN c_general_options-max_rows
      ELSE cv_option
    ).
  ENDMETHOD.

ENDCLASS.
