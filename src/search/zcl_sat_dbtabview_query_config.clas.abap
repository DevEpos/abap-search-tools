"! <p class="shorttext synchronized" lang="en">Configuration for Database Table/View Query</p>
CLASS zcl_sat_dbtabview_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS zif_sat_object_search_config~get_type
        REDEFINITION .
    METHODS zif_sat_object_search_config~map_option
        REDEFINITION.
  PROTECTED SECTION.
    ALIASES:
      c_dbtab_options FOR zif_sat_c_object_search~c_dbtab_search_params.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_dbtabview_query_config IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mt_options = VALUE #(
      ( option = c_general_options-package allowed_length = 30 )
      ( option = c_general_options-user allowed_length = 12 )
      ( option = c_dbtab_options-field  allowed_length = 30 )
      ( option = c_general_options-type )
      ( option = c_dbtab_options-delivery_class allowed_length = 1 )
      ( option = c_general_options-description allowed_length = 40 )
      ( option = c_general_options-max_rows single = abap_true no_negation = abap_true )
    ).
  ENDMETHOD.


  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-db_tab_view.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~map_option.
    cv_option = SWITCH #( cv_option
      WHEN c_search_option-by_package     THEN c_general_options-package
      WHEN c_search_option-by_owner       THEN c_general_options-user
      WHEN c_search_option-by_field       THEN c_dbtab_options-field
      WHEN c_search_option-by_type        THEN c_general_options-type
      WHEN c_search_option-by_description THEN c_general_options-description
      WHEN c_search_option-max_rows       THEN c_general_options-max_rows
      ELSE cv_option
    ).
  ENDMETHOD.
ENDCLASS.
