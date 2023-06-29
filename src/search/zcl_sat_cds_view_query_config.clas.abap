"! <p class="shorttext synchronized">Configuration for CDS View Query</p>
CLASS zcl_sat_cds_view_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    ALIASES c_cds_options FOR zif_sat_c_object_search~c_cds_search_params.
ENDCLASS.


CLASS zcl_sat_cds_view_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_t_query_filter(
        ( get_package_filt_conf( ) )
        ( get_user_filt_conf( ) )
        ( get_rel_state_filt_conf( ) )
        ( get_description_filt_conf( ) )
        ( get_max_rows_filt_conf( ) )
        ( name           = c_cds_options-params
          single         = abap_true
          no_negation    = abap_true
          data_type      = zif_sat_c_object_search=>c_filter_data_type-boolean )
        ( name = c_cds_options-param )
        ( name           = c_cds_options-select_from
          allowed_length = 30
          patterns       = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-db_entity ) )
        ( name           = c_cds_options-association
          allowed_length = 30
          patterns       = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-db_entity ) )
        ( name           = c_cds_options-annotation
          key_value      = abap_true
          patterns       = abap_true
          content_assist = VALUE #(
              assist_type             = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
              category_scheme         = zif_sat_c_object_search=>c_content_assist-category_scheme
              category_term           = zif_sat_c_object_search=>c_content_assist-terms-annotation
              secondary_category_term = zif_sat_c_object_search=>c_content_assist-terms-annotatio_value ) )
        ( name           = c_cds_options-field
          allowed_length = 30
          patterns       = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_field ) )
        ( name           = c_general_options-type
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_type ) )
        ( name           = c_cds_options-extended_by
          allowed_length = 30
          patterns       = abap_true
          no_negation    = abap_true
          content_assist = VALUE #( assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                                    category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
                                    category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_extension ) )
        ( name           = c_cds_options-only_local_assocs
          single         = abap_true
          data_type      = zif_sat_c_object_search=>c_filter_data_type-boolean
          internal       = abap_true ) ).

    mt_options = lt_object_filters.

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_s_search_type(
                               label  = 'CDS View'
                               name   = zif_sat_c_object_search=>c_search_type-cds_view
                               inputs = VALUE #( ( name    = c_object_name_input_key
                                                   label   = c_object_name_input_label )
                                                 ( name    = c_object_filter_input_key
                                                   label   = c_object_filter_input_label
                                                   filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-cds_view.
  ENDMETHOD.
ENDCLASS.
