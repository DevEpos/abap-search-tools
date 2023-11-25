CLASS zcl_sat_adt_discovery_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_utils_root_scheme TYPE string VALUE 'http://www.devepos.com/adt/saat'.
    CONSTANTS c_utils_root_scheme_v2 TYPE string VALUE 'http://www.devepos.com/adt/saat/v2'.
    CONSTANTS c_utils_rel_scheme TYPE string VALUE 'http://www.devepos.com/adt/relations/saat'.
    CONSTANTS c_utils_rel_scheme_v2 TYPE string VALUE 'http://www.devepos.com/adt/relations/saat/v2'.
    CONSTANTS c_object_search_uri TYPE string VALUE '/objectsearch'.
    CONSTANTS c_sapaox_launcher_uri TYPE string VALUE '/sapaox'.
    CONSTANTS c_ddic_repo_access_uri TYPE string VALUE '/ddicaccess'.
    CONSTANTS c_nav_targets_uri TYPE string VALUE '/navigationtargets'.
    CONSTANTS c_static_uri TYPE string VALUE '/devepos/adt/saat'.
    CONSTANTS c_app_title TYPE string VALUE 'Discovery Provider for ABAP Search and Analysis Tools'.

    CONSTANTS:
      BEGIN OF c_cds_analysis_uri,
        base                  TYPE string VALUE '/cds/analysis',
        top_down_analysis     TYPE string VALUE '/cds/analysis/topDown',
        where_used_base       TYPE string VALUE '/cds/analysis/whereUsedIn',
        where_used_in_from    TYPE string VALUE '/from',
        where_used_in_assoc   TYPE string VALUE '/associations',
        used_entites_analysis TYPE string VALUE '/cds/analysis/usedEntities',
        field_hierarchy       TYPE string VALUE '/cds/analysis/field/hierarchy',
        field_where_used      TYPE string VALUE '/cds/analysis/field/whereUsed',
      END OF c_cds_analysis_uri.

    CONSTANTS:
      BEGIN OF c_element_info_uri,
        base   TYPE string VALUE '/elementinfo',
        by_uri TYPE string VALUE '/byUri',
      END OF c_element_info_uri.

    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.

  PROTECTED SECTION.
    METHODS get_application_title REDEFINITION.
    METHODS register_resources    REDEFINITION.
    METHODS fill_router           REDEFINITION.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Register object search</p>
    METHODS register_object_search_res
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Register value help providers</p>
    METHODS register_value_help_providers
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Registers element info for tree expansion</p>
    METHODS register_element_info
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Registers resources for SAP Analyis for Office Tools</p>
    METHODS register_sapaox_launcher
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Registers Resources for CDS Analysis Tools</p>
    METHODS register_cds_analysis
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Registers Resources for detecting navigation targets</p>
    METHODS register_navigation_targets
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Registers resource for DDIC Repository Access</p>
    METHODS register_ddic_repo_access
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
ENDCLASS.


CLASS zcl_sat_adt_discovery_app IMPLEMENTATION.
  METHOD fill_router.
    super->fill_router( CHANGING router = router ).
    router->attach( iv_template      = '/discovery'
                    iv_handler_class = cl_adt_res_discovery=>co_class_name ).
  ENDMETHOD.

  METHOD get_application_title.
    result = c_app_title.
  ENDMETHOD.

  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = c_static_uri.
  ENDMETHOD.

  METHOD register_resources.
    register_object_search_res( registry ).
    register_value_help_providers( registry ).
    register_element_info( registry ).
    register_sapaox_launcher( registry ).
    register_cds_analysis( registry ).
    register_navigation_targets( registry ).
    register_ddic_repo_access( io_registry = registry ).
  ENDMETHOD.

  METHOD register_object_search_res.
    io_registry->register_discoverable_resource( url             = c_object_search_uri
                                                 handler_class   = 'ZCL_SAT_ADT_RES_OBJECT_SEARCH'
                                                 description     = 'ABAP Object Search'
                                                 category_scheme = |{ c_utils_root_scheme_v2 }{ c_object_search_uri }|
                                                 category_term   = 'objectSearch' ).
    io_registry->register_discoverable_resource( url             = |{ c_object_search_uri }/config|
                                                 handler_class   = 'ZCL_SAT_ADT_RES_SEARCH_CONFIG'
                                                 description     = 'ABAP Object Search Config'
                                                 category_scheme = |{ c_utils_root_scheme_v2 }{ c_object_search_uri }|
                                                 category_term   = 'objectSearchConfig' ).
  ENDMETHOD.

  METHOD register_element_info.
    CONSTANTS lc_eleminfo_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_ELEMENT_INFO'.

    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
                                           url             = c_element_info_uri-base
                                           handler_class   = lc_eleminfo_handler
                                           description     = 'Element info provider'
                                           category_scheme = c_utils_root_scheme_v2 && c_element_info_uri-base
                                           category_term   = 'elementinfo' ).

    " Register URI template for element information resource
    lo_element_info_collection->register_disc_res_w_template(
        template      = |{ c_element_info_uri-base }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name },| &&
                        |{ zif_sat_c_adt_utils=>c_element_info_parameter-object_type }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_cds_elem_info_parameter-show_association_name }*\}|
        handler_class = lc_eleminfo_handler
        relation      = c_utils_rel_scheme_v2 && c_element_info_uri-base ).

    " Register resource template for element info by uri
    lo_element_info_collection->register_disc_res_w_template(
        template      = |{ c_element_info_uri-base }{ c_element_info_uri-by_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-uri }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_cds_elem_info_parameter-show_association_name }*\}|
        handler_class = 'ZCL_SAT_ADT_RES_ELEMINFO_BYURI'
        relation      = c_utils_rel_scheme_v2 && c_element_info_uri-base && c_element_info_uri-by_uri ).
  ENDMETHOD.

  METHOD register_value_help_providers.
    io_registry->register_discoverable_resource(
        url             = '/applcomp'
        handler_class   = 'ZCL_SAT_ADT_RES_APPLC_VH'
        description     = 'Search for Application Component'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-appl_comp ).

    io_registry->register_discoverable_resource(
        url             = '/softwarecomp'
        handler_class   = 'ZCL_SAT_ADT_RES_SWCOMP_VH'
        description     = 'Search for Software Component'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-software_comp ).

    io_registry->register_discoverable_resource(
        url             = '/cdsfield'
        handler_class   = 'ZCL_SAT_ADT_RES_CDSFIELD_VH'
        description     = 'Search for CDS View field.'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_field ).

    io_registry->register_discoverable_resource(
        url             = '/cdsbasefield'
        handler_class   = 'ZCL_SAT_ADT_RES_CDSBASEFLD_VH'
        description     = 'Search for CDS View Base field.'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_base_field ).

    io_registry->register_discoverable_resource(
        url             = '/cdsparam'
        handler_class   = 'ZCL_SAT_ADT_RES_CDS_PARAM_VH'
        description     = 'Search for CDS View Parameter.'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_param ).

    io_registry->register_discoverable_resource(
        url             = '/tablefield'
        handler_class   = 'ZCL_SAT_ADT_RES_TABFIELD_VH'
        description     = 'Search for Table Field'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_field ).

    io_registry->register_discoverable_resource(
        url             = '/annotation'
        handler_class   = 'ZCL_SAT_ADT_RES_ANNO_VH'
        description     = 'Search for Annotation'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-annotation ).

    io_registry->register_discoverable_resource(
        url             = '/annotationvalue'
        handler_class   = 'ZCL_SAT_ADT_RES_ANNO_VALUE_VH'
        description     = 'Search for Annotation Values'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-annotatio_value ).

    io_registry->register_discoverable_resource(
        url             = '/dbentity'
        handler_class   = 'ZCL_SAT_ADT_RES_DB_ENTITY_VH'
        description     = 'Search for Database entities'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-db_entity ).

    io_registry->register_discoverable_resource(
        url             = '/releasestate'
        handler_class   = 'ZCL_SAT_ADT_RES_RELSTATE_VH'
        description     = 'Search for Release API states'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-release_state ).

    io_registry->register_discoverable_resource(
        url             = '/cdstype'
        handler_class   = 'ZCL_SAT_ADT_RES_CDS_TYPE_VH'
        description     = 'Search for CDS source types'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_type ).

    io_registry->register_discoverable_resource(
        url             = '/cdsextension'
        handler_class   = 'ZCL_SAT_ADT_RES_CDS_EXT_VH'
        description     = 'Search for CDS Extension'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-cds_extension ).

    io_registry->register_discoverable_resource(
        url             = '/abaplanguage'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_ALANG_VH'
        description     = 'Resource for available ABAP Languages'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-abap_language ).

    io_registry->register_discoverable_resource(
        url             = '/classcategory'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_CATEG_VH'
        description     = 'Resource for ABAP Class Categories'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_category ).

    io_registry->register_discoverable_resource(
        url             = '/classflag'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_FLAG_VH'
        description     = 'Resource for ABAP Class Flags'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_flag ).

    io_registry->register_discoverable_resource(
        url             = '/classtype'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_TYPE_VH'
        description     = 'Resource for Class Type'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-class_type ).

    io_registry->register_discoverable_resource(
        url             = '/dbdelivclass'
        handler_class   = 'ZCL_SAT_ADT_RES_DB_DLVCLASS_VH'
        description     = 'Resource for Delivery Classes'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_deliv_class ).

    io_registry->register_discoverable_resource(
        url             = '/dbdataclass'
        handler_class   = 'ZCL_SAT_ADT_RES_DB_DATACLS_VH'
        description     = 'Resource for Table Data Class'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-table_data_class ).

    io_registry->register_discoverable_resource(
        url             = '/viewfield'
        handler_class   = 'ZCL_SAT_ADT_RES_VIEWFIELD_VH'
        description     = 'Resource for View Field'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-view_field ).

    io_registry->register_discoverable_resource(
        url             = '/viewroottable'
        handler_class   = 'ZCL_SAT_ADT_RES_VIEWROOT_VH'
        description     = 'Resource for root tables of dictionary views'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-view_root_tab ).

    io_registry->register_discoverable_resource(
        url             = '/viewbasetable'
        handler_class   = 'ZCL_SAT_ADT_RES_VIEWBASET_VH'
        description     = 'Resource for base tables of dictionary views'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-view_base_tab ).

    io_registry->register_discoverable_resource(
        url             = '/dbtabinclude'
        handler_class   = 'ZCL_SAT_ADT_RES_DBTABINCL_VH'
        description     = 'Resource for includes in a db table'
        category_scheme = zif_sat_c_object_search=>c_content_assist-category_scheme
        category_term   = zif_sat_c_object_search=>c_content_assist-terms-db_tab_include ).
  ENDMETHOD.

  METHOD register_sapaox_launcher.
    CONSTANTS lc_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_AOX_LAUNCHER'.

    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
                                           url             = c_sapaox_launcher_uri
                                           handler_class   = lc_handler
                                           description     = 'Analysis for Office Launcher'
                                           category_scheme = c_utils_root_scheme && c_sapaox_launcher_uri
                                           category_term   = 'sapaox' ).

    " Build template for Analysis for Office Launcher
    DATA(lv_template) = |{ c_sapaox_launcher_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name }\}|.

    " Register URI template for Analyis for Office launcher
    lo_element_info_collection->register_disc_res_w_template(
        template      = lv_template
        handler_class = lc_handler
        relation      = c_utils_rel_scheme && c_sapaox_launcher_uri ).
  ENDMETHOD.

  METHOD register_cds_analysis.
    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
                                           url             = c_cds_analysis_uri-base
                                           handler_class   = 'ZCL_SAT_ADT_RES_CDS_ANALYSIS'
                                           description     = 'Resource for CDS Analysis'
                                           category_scheme = c_utils_root_scheme_v2 && c_cds_analysis_uri-base
                                           category_term   = 'cdsanalysis' ).

    " Register URI template for Top-Down Analysis
    lo_element_info_collection->register_disc_res_w_template(
        template      = c_cds_analysis_uri-top_down_analysis &&
                        |\{?{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-cds_name }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-with_associations }*\}|
        handler_class = 'ZCL_SAT_ADT_RES_CDS_A_TOPDOWN'
        relation      = c_utils_rel_scheme_v2 && c_cds_analysis_uri-top_down_analysis ).

    " Register URI templates for Where-Used-Analysis
    DATA(lv_where_used_in_template) = c_cds_analysis_uri-where_used_base &&
        |\{?{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-entity_name }\}| &&
        |\{&{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-source_origin }*\}| &&
        |\{&{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_local_assocs }*\}| &&
        |\{&{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-recursive_search }*\}| &&
        |\{&{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-only_released_entities }*\}|.

    lo_element_info_collection->register_disc_res_w_template(
        template      = lv_where_used_in_template
        handler_class = 'ZCL_SAT_ADT_RES_CDS_A_WUSL'
        relation      = c_utils_rel_scheme_v2 && c_cds_analysis_uri-where_used_base && c_cds_analysis_uri-where_used_in_from ).

    lo_element_info_collection->register_disc_res_w_template(
        template      = lv_where_used_in_template
        handler_class = 'ZCL_SAT_ADT_RES_CDS_A_WUSL'
        relation      = c_utils_rel_scheme_v2 && c_cds_analysis_uri-where_used_base && c_cds_analysis_uri-where_used_in_assoc ).

    " Register URI template for Used Entities Analyis
    lo_element_info_collection->register_disc_res_w_template(
        template      = c_cds_analysis_uri-used_entites_analysis &&
                        |\{?{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-cds_name }\}|
        handler_class = 'ZCL_SAT_ADT_RES_CDS_A_USED_ENT'
        relation      = c_utils_rel_scheme_v2 && c_cds_analysis_uri-used_entites_analysis ).

    " Register template for column where-used list
    lo_element_info_collection->register_disc_res_w_template(
        template      = c_cds_analysis_uri-field_where_used &&
                        |\{?{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-name }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-field }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_calc_fields }*\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_db_views }*\}|
        handler_class = 'ZCL_SAT_ADT_RES_COL_WHERE_USED'
        relation      = c_utils_rel_scheme_v2 && c_cds_analysis_uri-field_where_used ).

    " Register template for column hierarchy
    lo_element_info_collection->register_disc_res_w_template(
        template      = c_cds_analysis_uri-field_hierarchy &&
                        |\{?{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-name }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-field }\}|
        handler_class = 'ZCL_SAT_ADT_RES_COL_HIERARCHY'
        relation      = c_utils_rel_scheme_v2 && c_cds_analysis_uri-field_hierarchy ).
  ENDMETHOD.

  METHOD register_navigation_targets.
    CONSTANTS lc_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_NAV_TARGETS'.

    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
                                           url             = c_nav_targets_uri
                                           handler_class   = lc_handler
                                           description     = 'Resource for Navigation targets'
                                           category_scheme = c_utils_root_scheme_v2 && c_nav_targets_uri
                                           category_term   = 'navigationtargets' ).

    lo_element_info_collection->register_disc_res_w_template(
        template      = |{ c_nav_targets_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_element_info_parameter-object_type }\}|
        handler_class = lc_handler
        relation      = c_utils_rel_scheme_v2 && c_nav_targets_uri ).
  ENDMETHOD.

  METHOD register_ddic_repo_access.
    CONSTANTS lc_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_DDIC_REP_ACC'.

    " Register resource for DDIC Repository access
    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
                                           url             = c_ddic_repo_access_uri
                                           handler_class   = lc_handler
                                           description     = 'Resource for Repository DDIC Access'
                                           category_scheme = c_utils_root_scheme_v2 && c_ddic_repo_access_uri
                                           category_term   = 'ddicAccess' ).

    lo_element_info_collection->register_disc_res_w_template(
        template      = |{ c_ddic_repo_access_uri }\{?{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-access_mode }\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-uri }*\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-paths }*\}| &&
                        |\{&{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-filters }*\}|
        handler_class = lc_handler
        relation      = c_utils_rel_scheme_v2 && c_ddic_repo_access_uri ).
  ENDMETHOD.
ENDCLASS.
