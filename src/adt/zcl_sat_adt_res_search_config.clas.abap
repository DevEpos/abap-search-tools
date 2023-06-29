"! <p class="shorttext synchronized">Resource for reading the object search configuration</p>
CLASS zcl_sat_adt_res_search_config DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES ty_search_providers TYPE STANDARD TABLE OF REF TO zif_sat_object_search_config WITH EMPTY KEY.

    METHODS get_config_providers
      RETURNING
        VALUE(result) TYPE ty_search_providers.
ENDCLASS.


CLASS zcl_sat_adt_res_search_config IMPLEMENTATION.
  METHOD get.
    DATA ls_search_config TYPE zif_sat_ty_object_search=>ty_s_search_config.

    LOOP AT get_config_providers( ) INTO DATA(lo_provider).
      ls_search_config-search_types = VALUE #( BASE ls_search_config-search_types ( lo_provider->get_search_config( ) ) ).
    ENDLOOP.

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_search_config_ch( )
                             data            = ls_search_config ).
  ENDMETHOD.

  METHOD get_config_providers.
    result = VALUE #( ( NEW zcl_sat_dbtabview_query_config( ) )
                      ( NEW zcl_sat_cds_view_query_config( ) )
                      ( NEW zcl_sat_clsintf_query_config( ) )
*                      ( NEW zcl_sat_clif_meth_query_config( ) )
                      ).
  ENDMETHOD.
ENDCLASS.
