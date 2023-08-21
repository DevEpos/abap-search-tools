"! <p class="shorttext synchronized">Configuration for Database Table/View Query</p>
CLASS zcl_sat_dbtab_query_config DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_query_config
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_sat_object_search_config~get_type REDEFINITION.

  PROTECTED SECTION.
    ALIASES c_dbtab_options FOR zif_sat_c_object_search~c_dbtab_search_params.

    METHODS build_config REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        transport      TYPE string VALUE 'ABAP:IMG_TRANSPORT',
        buffering      TYPE string VALUE 'ABAP:IMG_BUFFERING',
        buffering_type TYPE string VALUE 'ABAP:IMG_BUFFERING_TYPE',
        data_class     TYPE string VALUE 'ABAP:IMG_DATA_CLASS',
        size_category  TYPE string VALUE 'ABAP:IMG_SIZE_CATEGORY',
      END OF c_image_keys.

    METHODS get_field_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_deliv_class_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_data_class_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_buffering_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_buffering_type_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_size_category_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_flag_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_enhancement_cat_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_maintenance_filter
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.
ENDCLASS.


CLASS zcl_sat_dbtab_query_config IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    build_config( ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_type.
    rv_type = zif_sat_c_object_search=>c_search_type-db_tab.
  ENDMETHOD.

  METHOD build_config.
    DATA(lt_object_filters) = VALUE zif_sat_ty_object_search=>ty_query_filters( ( get_user_filt_conf( ) )
                                                                                ( get_created_on_filt_conf( ) )
                                                                                ( get_changed_by_filt_conf( ) )
                                                                                ( get_changed_on_filt_conf( ) )
                                                                                ( get_package_filt_conf( ) )
                                                                                ( get_softw_comp_filt_conf( ) )
                                                                                ( get_appl_comp_filt_conf( ) )
                                                                                ( get_description_filt_conf( ) )
                                                                                ( get_max_rows_filt_conf( ) )
                                                                                ( get_field_filter( ) )
                                                                                ( get_deliv_class_filter( ) )
                                                                                ( get_enhancement_cat_filter( ) )
                                                                                ( get_maintenance_filter( ) )
                                                                                ( get_flag_filter( ) )
                                                                                ( get_buffering_filter( ) )
                                                                                ( get_buffering_type_filter( ) )
                                                                                ( get_data_class_filter( ) )
                                                                                ( get_size_category_filter( ) ) ).

    ms_search_type = VALUE zif_sat_ty_object_search=>ty_search_type_config(
        label    = 'Database Table'
        name     = zif_sat_c_object_search=>c_search_type-db_tab
        img_info = VALUE #( img_key      = zif_sat_c_object_types=>table_definition
                            img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        inputs   = VALUE #( ( name    = zif_sat_c_object_search=>c_search_fields-object_name_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_name_input_label )
                            ( name    = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                              label   = zif_sat_c_object_search=>c_search_fields-object_filter_input_label
                              filters = lt_object_filters ) ) ).

    mt_options = lt_object_filters.
  ENDMETHOD.

  METHOD get_deliv_class_filter.
    result = VALUE #(
        name             = c_dbtab_options-delivery_class
        description      = 'Delivery Class'
        long_description = |Use '{ c_dbtab_options-delivery_class }' to search for Tables with specific delivery classes.\n\n| &&
                           |Example:\n   { c_dbtab_options-delivery_class } : A|
        img_info         = VALUE #(
            img_key     = c_image_keys-transport
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABXElEQVR4nKWRXUvCcBTG/WBdlSBEEQRCJERvBN3UTYRERGWElJpRWpRSggZtZS+uV4Vd1G2Us75Ca5pGb2tq9bT9w6lbLcIDP3ZxHn47` &&
                          `5/xNplqLPXTiLwwFpzEXLrLAuQCc/UCcmTUWxKLzJJiRgGxBD7PjNRbQlJ8IcnL4sahni/YZC8KRVSJ4kMPP73oiGytE0DVzjF7XCfo8cfR7ExhcZJuIIBBaJ4InWSB+6AmG1lTBZbZIYK7TiuB7Ml8g` &&
                          `QgQv8t/yn3r8wXBZcF9QUQXuJQruZQqeKjar0K0wl8DAAmt8G21VCpRSBeZhChY7jcaRbTSPRtEytovW8T1YJ/fRNsXANn2ADucRfp1AESg7XZXI5JFMS0gKEjjhDdydCI4XkeJfkboto96gNIG5cwJ1` &&
                          `1iHy1U5g6XGQnqXbUfmM1TdQAnw6R4La/Y16ajXY7JwSqG+33/ynV3N9AVC53MX4NLXNAAAAAElFTkSuQmCC` )
        allowed_length   = 1
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            caching               = abap_true
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-table_deliv_class
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_field_filter.
    result = VALUE #(
        name             = c_dbtab_options-field
        description      = 'Table Field'
        long_description = |Use '{ c_dbtab_options-field }' to restrict the search query by certain Fields.\n\n| &&
             |Example:\n   { c_dbtab_options-field } : devclass|
        img_info         = VALUE #( img_key      = zif_sat_c_object_types=>table_field
                                    img_registry = zif_sat_c_object_search=>c_image_registry_id-adt_type )
        patterns         = abap_true
        allowed_length   = 30
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-table_field
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_buffering_filter.
    result = VALUE #(
        name             = c_dbtab_options-buffering
        description      = 'Buffering Status'
        long_description = |Use '{ c_dbtab_options-buffering }' to restrict the query by the buffering status of tables.\n\n| &&
                           |Example:\n   { c_dbtab_options-buffering } : off|
        img_info         = VALUE #(
            img_key     = c_image_keys-buffering
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAAAHUAAAB1AePCB2UAAAL+SURBVDhPpZNZaBNRFIbPTJLJ1iyTNEmbpia0` &&
                          `dkndGsUFpLg9iCu+VKs+CCIuYEFRRETBB32yCIKKG7aCGK2lLqiIqBW1bVqxLVZNW40xbZqQdJk0ncw0mXTGmTFSbH3zg8uZe5afc+69A/8LkrEzOXIn2yjXbmdRhCWI4btwaddIJvIXMwUO1+dZNfie` &&
                          `+Q7HCkwqVakVivljVMLT5fv+JkQQN+Di9lAmU2RK4JDb4TDm7HLabKtQFJHRSYYx6/TFCAK5mQyWSiY/fAr43/nDketwqapPcIoC2Sefn1peXLJRIpPKEzQdN2RllfEhoxCbRiLNskEmnYo393gfDZ1Z` &&
                          `fxYVvMNjscfeweA7dnKSMmRpKqYXcxyQYxTZPkqS7SmGGfdFoh1DJPFUiE2NsPeqDJS68iKLdWtZfn6FSq5YwnvTI+S4R4pKUB7EG+xv8UXC9UCPdcG1fYxQJgpoTzypctnt2+QSTN0V8L2MxuNNdpNl` &&
                          `8yYTukPDUdp0ih5WQjo8waEmCaYycxybRhg6aJQhN6WiCss3yWPQahatmedaGaMSzd0/fzTpJmKDW3ZWFjAsZPPhUiFHgGVZweR6Gh90SoSv5Hv3l4C1vNE/HB9QYjLOrMMX2c2WjTg7biwsKcDy1QBG` &&
                          `BQAu/71ePn8NFg0WDwUGP4uHCNVuV2nh3JoSm3W151vvrcb2lmp/JOzme+NH5ECGcqDke8VQFupuP4TWtk7IzdaKZyB2YFq7e8/CgtlrcvT4Bqctv0IhxVSdAf/D5SpqFiJX2Pu/9UGRwwq17mfQ7fXB` &&
                          `8YM7QK/TTXZ/6u0WBai3d9725C1rIidoyqDW4Dk4vs6ZZ1upZwhTsbNIXVd7D9o+foWBUBTOnz4AZiMOmBQd/djh7RUFRNobiFjTrVd99qVPiQQZ0SiVGhuSmLPA5UQWl9jga893OLq/Eix8McoPhvBv` &&
                          `4wMvkKn+B/zPdOxCQ1drlOOIFMvFEjSX5G/rz5pIs6Gayw1Xph7SPzh3+f4x3iz7vZsJAsiLX5CxO3M0nRwdAAAAAElFTkSuQmCC` )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = VALUE #(
                ( name        = zif_sat_c_object_search=>c_db_buffer_status-ext-on
                  description = 'Buffering switched on' )
                ( name        = zif_sat_c_object_search=>c_db_buffer_status-ext-allowed_but_off
                  description = 'Buffering allowed but switched off' )
                ( name        = zif_sat_c_object_search=>c_db_buffer_status-ext-off
                  description = 'Buffering switched off' ) ) ) ).
  ENDMETHOD.

  METHOD get_buffering_type_filter.
    result = VALUE #(
        name             = c_dbtab_options-buffering_type
        description      = 'Buffering Type'
        long_description = |Use '{ c_dbtab_options-buffering_type }' to restrict the query by the buffering type of tables.\n\n| &&
                           |Example:\n   { c_dbtab_options-buffering_type } : full|
        img_info         = VALUE #(
            img_key     = c_image_keys-buffering_type
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAB2AAAAdgFOeyYIAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAhRJREFUOI11k09I03EYxj/Pz82E` &&
                          `UBqCZSCW66CgMjfYpUVJdFoXTzGPSdAfoT8HF0GlEdFOHWQRXbqllyDKIqwEoWEkTIOgf5SKFjFakbMI8+fbYYrTzff4fp/nfZ/n5fmKTcoG64OYcxzkgntbsZmJUjgVEe82RJDFQVF2RERmDJaXAFI4` &&
                          `6tWRz89KDlgh9iLnILX7wdcC30Yh+3rjjhRGgtjUkITJBuo7wLmI42mjLgpVDTAzBPMfN3O3ujuNuCwb2D0MHKLKD+4i/J7dnLOtCXa2w9zT1QXPtd430VJ3wdcMtfsgNw2zTwBLYSTUOfUwP+BUZTUe` &&
                          `mghXL+DoHNAJlOF4ofEYfJ+AzJgBj8Cu8jLrZYm3SuaysrOV1zG6ga3AK8Q12mo+4V3uAWKAA9zD3D7Gf+7BuACEgQXQDQ+mRbBVsWGM+6QzbzAl2OWLU+E6vJ8/ADYINK/3Zp41C16dRtaN4StAuCs3` &&
                          `cQp6P0D9/LN+JXPZ/MOLhlZG/b94t70V6QzwZQVcVkDOgPqYrg4x6s+R8rcAyILBEcza84osh+PcoubPTRrnohhdSOVgST7UPebrlpPACaTKfBQ0LAsEokh9QKhA5l/gDtIDzJaQOjA7ClQUYMaRrqxF` &&
                          `ORSK4LpxpMNFOVhfKaQE6fSQwIo/UyCwFziPtDFUKaRLSqdHCvHFqVtTFMSsC7NyzJKanJwshfsPV9i/2rGYQQIAAAAASUVORK5CYII=` )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = VALUE #(
                ( name        = zif_sat_c_object_search=>c_db_buffer_type-ext-no_buffering
                  description = 'No Buffering' )
                ( name        = zif_sat_c_object_search=>c_db_buffer_type-ext-single_entries
                  description = 'Single entries of table were buffered' )
                ( name        = zif_sat_c_object_search=>c_db_buffer_type-ext-full_with_gen_key
                  description = 'Full buffering as specified by generic key' )
                ( name        = zif_sat_c_object_search=>c_db_buffer_type-ext-full_table
                  description = 'Full table is passed to buffer' ) ) ) ).
  ENDMETHOD.

  METHOD get_data_class_filter.
    result = VALUE #(
        name             = c_dbtab_options-data_class
        description      = 'Data Class'
        long_description = |Use '{ c_dbtab_options-data_class }' to restrict the query by the data class of a table.\n\n| &&
                           |Example:\n   { c_dbtab_options-data_class } : appl2|
        img_info         = VALUE #(
            img_key     = c_image_keys-data_class
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAADiSURBVDhPYxhwwAilMUD2ntf/P/xmArMFWP8x` &&
                          `THURxaoWpwEqEy/+hzLB4E6+Pla1ECuwgI8vHkFZqGx0gMsA7jBnC7BGEAaxgYAFLEMiAHkDxSvoAO6viIgIvArRwYoVK1DDBJsBIDFc4lAmIgyAJkJZhAGyWkZQfN96+xPKZWBQE2YHx3n09rf/X336` &&
                          `DhYT4+NkWOopjFUty86rz6BcCLj/AkKfvPUEwgACmBgI3H/xFsoCAWEGJvQ4hvGRxWHsVXtPoMhfuHCegQk5vkEYGudgGk2MGSYHEgMBGH8gAQMDACiDc2au/e4oAAAAAElFTkSuQmCC` )
        content_assist   = VALUE #(
            caching               = abap_true
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-table_data_class ) ).
  ENDMETHOD.

  METHOD get_enhancement_cat_filter.
    result = VALUE #(
        name             = c_dbtab_options-enhancement_category
        description      = 'Enhancement Category'
        long_description = |Use '{ c_dbtab_options-enhancement_category }' to find tables by their enhancement categories.\n\n| &&
                           |Example:\n   { c_dbtab_options-enhancement_category } : not_enhanced|
        img_info         = VALUE #( img_key     = c_general_image_keys-extension
                                    img_encoded = get_general_image( c_general_image_keys-extension ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = zcl_sat_table_filter_values=>get_enhancecat_filt_values( ) ) ).
  ENDMETHOD.

  METHOD get_flag_filter.
    result = VALUE #(
        name             = c_dbtab_options-flag
        description      = 'Flag'
        long_description = |Use '{ c_dbtab_options-flag }' to find tables with certain options.\n\n| &&
                           |Example:\n   { c_dbtab_options-flag } : client_dep|
        img_info         = VALUE #( img_key     = c_general_image_keys-checked_box
                                    img_encoded = get_general_image( c_general_image_keys-checked_box ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = VALUE #(
                ( name = zif_sat_c_object_search=>c_db_flags-client_dep        description = 'Client Dependent' )
                ( name = zif_sat_c_object_search=>c_db_flags-used_in_shlp      description = 'Used in Search Help' )
                ( name = zif_sat_c_object_search=>c_db_flags-change_log_active description = 'Changes to table entries are logged' ) ) ) ).
  ENDMETHOD.

  METHOD get_size_category_filter.
    result = VALUE #(
        name             = c_dbtab_options-size_category
        description      = 'Size Category'
        long_description = |Use '{ c_dbtab_options-size_category }' to restrict the query by the size category of a table.\n\n| &&
                           |Example:\n   { c_dbtab_options-size_category } : 0|
        img_info         = VALUE #(
            img_key     = c_image_keys-size_category
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAJkSURBVDhPpZNNaBNBFMff7EeSxmgKiU1rE0ih` &&
                          `NJqP5pM01NaDoHioIhVEL6mSkxG8terBngRPCoJHL8WD9GYPIlRBhWJLJdSaNDZIodFWbZoI2miTTXbXeZNE48dB6H+Znfd+8+bt7LwZ2KmITifoVVVFG1+ENg6duhpMqdvNvUIIAXJq6Gj5yMCQ5unc` &&
                          `JIxe6oaF1xvAcQS87jZ4lczROACfpw0WUzlQFBX8vRZIpjehUlEgMb++TS5Ez5R4oaqNRY000Azlsgw0MWg0PMzMrbEEAxErSJIMuFCtlofZl+vUV2Dm2WqZLZcQDqqyTC2VBnB0MmIVwoF21tBGhmNo` &&
                          `B70WiIQ6qE3nHu7rVwOOACyuPIex+CcG/1f3p2QVN6iXNpGRX9rT2brrynGvrSc22GPHX2pWNl9Ub06nPn6X5Dt/DIE+1GW66rWahq+fDDrbjS11XNPmVgluTafePk5/mEpk89coKjUSaMJd5lGfzXR2` &&
                          `/ITf2dmq/y3x529luP1kKfMw+f5RYrUwTtFWbYTugWufcUyvFeOXj3lsdvPu5jPAdG925d2DhWwmWyjeoO7XGv2pCjk37JI8fosoCBw4HSZWYzwHrv1mSL3Jsyj3ATMsLefZOfA490I6U4BqVYEX82sb` &&
                          `QrfdqMRjfnrcCKuxRuSB5wkc7LPCYMTGEiBv0Qkgyyoc6rcxrtJnI1cssSXrtAILQoWDHRDy1WqMrMGR4RgKGc5BkYtRs3z+NP/Xv/9LWE63Q4TkcoX5dycrEjEYBCPHsduEF0Sq9yhkjR6rgp8UR0aM` &&
                          `2xMTXwzoK/Q60X4nAvgBoRjJ5C+R0J8AAAAASUVORK5CYII=` )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = VALUE #( ( name = '0' description = 'Tables < 500K' )
                                             ( name = '1' description = 'Tables < 1.5 MB' )
                                             ( name = '2' description = 'Tables < 6.5 MB' )
                                             ( name = '3' description = 'Tables < 25 MB' )
                                             ( name = '4' description = 'Tables > 160 MB' )
                                             ( name = '5' description = '-' )
                                             ( name = '6' description = '-' )
                                             ( name = '7' description = '-' )
                                             ( name = '8' description = '-' )
                                             ( name = '9' description = '-' ) ) ) ).
  ENDMETHOD.

  METHOD get_maintenance_filter.
    result = VALUE #(
        name             = c_general_options-maintenance
        description      = 'Maintenance'
        long_description = |Use '{ c_general_image_keys-maintenance }' to restrict the query by the maintenance setting.\n\n| &&
                           |Example:\n   { c_general_image_keys-maintenance } : not_allowed|
        img_info         = VALUE #( img_key     = c_general_image_keys-maintenance
                                    img_encoded = get_general_image( c_general_image_keys-maintenance ) )
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-fixed_named_item
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter
            proposal_values       = zcl_sat_table_filter_values=>get_maintenance_filt_values( ) ) ).
  ENDMETHOD.
ENDCLASS.
