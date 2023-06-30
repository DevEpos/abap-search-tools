"! <p class="shorttext synchronized">Configuration for Database Table/View Query</p>
CLASS zcl_sat_dbtabview_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    ALIASES c_dbtab_options FOR zif_sat_c_object_search~c_dbtab_search_params.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_dbtabview_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter(
        ( get_package_filt_conf( ) )
        ( get_user_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( name           = c_dbtab_options-field
          allowed_length = 30
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_field ) )
        ( name           = c_general_options-type
          caching        = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_type ) )
        ( name           = c_dbtab_options-delivery_class
          allowed_length = 1
          caching        = abap_true
          content_assist = VALUE #(
              assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_deliv_class ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_s_search_type(
        label  = 'Database Table/View'
        name   = zif_sat_c_object_search=>c_search_type-db_tab_view
        inputs = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                            label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                          ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                            label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                            filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-db_tab_view.
  ENDMETHOD.
ENDCLASS.
