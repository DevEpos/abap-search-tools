"! <p class="shorttext synchronized" lang="en">Configuration for Database Table/View Query</p>
CLASS zcl_sat_dbtabview_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS zif_sat_object_search_config~get_type
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_dbtabview_query_config IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mt_options = VALUE #(
      ( option = c_search_option-by_owner allowed_length = 12 )
*      ( option = c_search_option-by_api )
      ( option = c_search_option-by_field allowed_length = 30 )
      ( option = c_search_option-by_type )
      ( option = c_search_option-by_package allowed_length = 30 )
      ( option = c_search_option-by_description allowed_length = 40 )
      ( option = c_search_option-max_rows single = abap_true no_negation = abap_true )
    ).
  ENDMETHOD.


  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-db_tab_view.
  ENDMETHOD.
ENDCLASS.
