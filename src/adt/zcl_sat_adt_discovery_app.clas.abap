CLASS zcl_sat_adt_discovery_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_utils_root_scheme TYPE string VALUE 'http://www.devepos.com/adt/saat'.
    CONSTANTS c_utils_rel_scheme TYPE string VALUE 'http://www.devepos.com/adt/relations/saat'.
    CONSTANTS c_object_search_uri TYPE string VALUE '/objectsearch'.
    CONSTANTS c_sapaox_launcher_uri TYPE string VALUE '/sapaox'.
    CONSTANTS c_db_fields_info_uri TYPE string VALUE '/dbfields/info'.
    CONSTANTS c_ddic_repo_access_uri TYPE string VALUE '/ddicaccess'.
    CONSTANTS c_column_info_uri TYPE string VALUE '/columninfo'.
    CONSTANTS c_column_hierarchy_uri TYPE string VALUE '/columninfo/hierarchy'.
    CONSTANTS c_column_where_used_uri TYPE string VALUE '/columninfo/whereUsed'.
    CONSTANTS c_cds_analysis_uri TYPE string VALUE '/cds/analysis'.
    CONSTANTS c_cds_top_down_analysis_uri TYPE string VALUE '/cds/analysis/topDown'.
    CONSTANTS c_cds_used_entites_analy_uri TYPE string VALUE '/cds/analysis/usedEntities'.
    CONSTANTS c_element_info_uri TYPE string VALUE '/elementinfo'.
    CONSTANTS c_element_info_by_uri_uri TYPE string VALUE '/elementinfoByUri'.
    CONSTANTS c_nav_targets_uri TYPE string VALUE '/navigationtargets'.
    CONSTANTS c_static_uri TYPE string VALUE '/devepos/adt/saat'.
    CONSTANTS c_app_title TYPE string VALUE 'Discovery Provider for ABAP Search and Analysis Tools'.

    METHODS if_adt_rest_rfc_application~get_static_uri_path
        REDEFINITION.
  PROTECTED SECTION.
    METHODS: get_application_title REDEFINITION,
      register_resources REDEFINITION,
      fill_router REDEFINITION.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Register object search</p>
    METHODS register_object_search_res
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Register value help providers</p>
    METHODS register_value_help_providers
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Registers element info for tree expansion</p>
    METHODS register_element_info
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Registers resources for SAP Analyis for Office Tools</p>
    METHODS register_sapaox_launcher
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Registers Resources for CDS Analysis Tools</p>
    METHODS register_cds_analysis
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Registers Resources for detecting navigation targets</p>
    METHODS register_navigation_targets
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Registers Resources for DB Field accesses</p>
    METHODS register_db_field_resources
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.
    "! <p class="shorttext synchronized" lang="en">Registers template for Object search</p>
    METHODS reg_object_search_template
      IMPORTING
        io_collection    TYPE REF TO if_adt_discovery_collection
        io_search_config TYPE REF TO zif_sat_object_search_config
        iv_handler_class TYPE string
        iv_search_type   TYPE zif_sat_ty_object_search=>ty_search_type.
    "! <p class="shorttext synchronized" lang="en">Registers resource for DDIC Repository Access</p>
    METHODS register_ddic_repo_access
      IMPORTING
        io_registry TYPE REF TO if_adt_disc_rest_rc_registry.

ENDCLASS.



CLASS zcl_sat_adt_discovery_app IMPLEMENTATION.

  METHOD fill_router.
    super->fill_router( CHANGING router = router ).
    router->attach(
        iv_template      = '/discovery'
        iv_handler_class = cl_adt_res_discovery=>co_class_name
    ).
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
    register_db_field_resources( registry ).
    register_ddic_repo_access( EXPORTING io_registry = registry ).
  ENDMETHOD.

  METHOD register_object_search_res.
    CONSTANTS: lc_object_search_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_OBJECT_SEARCH'.

    DATA(lo_search_collection) = io_registry->register_discoverable_resource(
        url             = c_object_search_uri
        handler_class   = lc_object_search_handler
        description     = 'Extended Object Search'
        category_scheme = c_utils_root_scheme && c_object_search_uri
        category_term   = 'search'
    ).

    reg_object_search_template( io_collection       = lo_search_collection
                                io_search_config    = CAST #( zcl_sat_ioc_lookup=>get_instance(
                                                                iv_contract = 'zif_sat_object_search_config'
                                                                iv_filter   = |{ zif_sat_c_object_search=>c_search_type-cds_view }| ) )
                                iv_handler_class    = 'ZCL_SAT_ADT_RES_OBJS_CDS'
                                iv_search_type      = zif_sat_c_object_search=>c_search_type-cds_view ).
    reg_object_search_template( io_collection       = lo_search_collection
                                io_search_config    = CAST #( zcl_sat_ioc_lookup=>get_instance(
                                                                iv_contract = 'zif_sat_object_search_config'
                                                                iv_filter   = |{ zif_sat_c_object_search=>c_search_type-db_tab_view }| ) )
                                iv_handler_class    = lc_object_search_handler
                                iv_search_type      = zif_sat_c_object_search=>c_search_type-db_tab_view ).
    reg_object_search_template( io_collection       = lo_search_collection
                                io_search_config    = CAST #( zcl_sat_ioc_lookup=>get_instance(
                                                                iv_contract = 'zif_sat_object_search_config'
                                                                iv_filter   = |{ zif_sat_c_object_search=>c_search_type-class_interface }| ) )
                                iv_handler_class    = lc_object_search_handler
                                iv_search_type      = zif_sat_c_object_search=>c_search_type-class_interface ).
  ENDMETHOD.

  METHOD reg_object_search_template.
    DATA: lt_handler_params TYPE abap_parmbind_tab.

    DATA(lt_allowed_options) = io_search_config->get_allowed_options( ).
    DATA(lv_template) = |{ c_object_search_uri }/{ to_lower( iv_search_type ) }\{?{ zif_sat_c_object_search=>c_general_search_params-object_name }*\}| &&
                        |\{&{ zif_sat_c_object_search=>c_general_search_params-get_all_results }*\}| &&
                        |\{&{ zif_sat_c_object_search=>c_general_search_params-read_api_state }*\}| &&
                        |\{&{ zif_sat_c_object_search=>c_general_search_params-use_and_for_filters }*\}| &&
                        |\{&{ zif_sat_c_object_search=>c_general_search_params-read_package_hierarchy }*\}|.

    DATA(lv_size) = lines( lt_allowed_options ).
    LOOP AT lt_allowed_options INTO DATA(ls_option).
      lv_template = lv_template && |\{&{ ls_option-low }*\}|.
    ENDLOOP.

*.. Creates parameter table for Resource Handler class
    DATA(ls_param) = VALUE abap_parmbind(
        kind = cl_abap_objectdescr=>exporting
        name = 'IV_SEARCH_TYPE'
    ).
    CREATE DATA ls_param-value LIKE iv_search_type.
    ASSIGN ls_param-value->* TO FIELD-SYMBOL(<lv_search_type>).
    <lv_search_type> = iv_search_type.
    INSERT ls_param INTO TABLE lt_handler_params.

    io_collection->register_disc_res_w_template(
      template      = lv_template
      handler_class = iv_handler_class
      parameters    = lt_handler_params
      relation      = 'http://www.devepos.com/adt/relations/saat/objectsearch/' && to_lower( iv_search_type )
    ).
  ENDMETHOD.

  METHOD register_element_info.
    CONSTANTS: lc_eleminfo_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_ELEMENT_INFO'.

    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
        url             = c_element_info_uri
        handler_class   = lc_eleminfo_handler
        description     = 'Element info provider'
        category_scheme = c_utils_root_scheme && c_element_info_uri
        category_term   = 'elementinfo'
    ).

*.. Register URI template for element information resource
    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_element_info_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name },| &&
                      |{ zif_sat_c_adt_utils=>c_element_info_parameter-object_type }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_cds_elem_info_parameter-show_association_name }*\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_element_info_parameter-basic_info }*\}|
      handler_class = lc_eleminfo_handler
      relation      = 'http://www.devepos.com/adt/relations/saat/elementinfo'
    ).

*.. Register resource template for element info by uri
    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_element_info_by_uri_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-uri }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_cds_elem_info_parameter-show_association_name }*\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_element_info_parameter-basic_info }*\}|
      handler_class = 'ZCL_SAT_ADT_RES_ELEMINFO_BYURI'
      relation      = 'http://www.devepos.com/adt/relations/saat/elementinfo/byUri'
    ).

*.. Register special URI template for CDS secondary element information retrieval
    lo_element_info_collection->register_disc_res_w_template(
      template      = '/elementinfo/cds/secondary' &&
                      |\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name }\}|
      handler_class = 'ZCL_SAT_ADT_RES_CDS_SECELINFO'
      relation      = 'http://www.devepos.com/adt/relations/saat/elementinfo/cds/secondary'
    ).
  ENDMETHOD.


  METHOD register_value_help_providers.

    DATA(lv_object_search_scheme) = c_utils_root_scheme && c_object_search_uri.

    io_registry->register_discoverable_resource(
        url             = '/cdsfield'
        handler_class   = 'ZCL_SAT_ADT_RES_CDSFIELD_VH'
        description     = 'Search for CDS View field.'
        category_scheme = lv_object_search_scheme
        category_term   = 'cdsfield'
    ).

    io_registry->register_discoverable_resource(
        url             = '/tablefield'
        handler_class   = 'ZCL_SAT_ADT_RES_TABFIELD_VH'
        description     = 'Search for Table Field'
        category_scheme = lv_object_search_scheme
        category_term   = 'tablefield'
    ).

    IF sy-saprl >= 752.
      io_registry->register_discoverable_resource(
          url             = '/annotation'
          handler_class   = 'ZCL_SAT_ADT_RES_ANNO_VH'
          description     = 'Search for Annotation'
          category_scheme = lv_object_search_scheme
          category_term   = 'annotation'
      ).

      io_registry->register_discoverable_resource(
          url             = '/annotationvalue'
          handler_class   = 'ZCL_SAT_ADT_RES_ANNO_VALUE_VH'
          description     = 'Search for Annotation Values'
          category_scheme = lv_object_search_scheme
          category_term   = 'annotationvalue'
      ).
    ENDIF.

    io_registry->register_discoverable_resource(
        url             = '/dbentity'
        handler_class   = 'ZCL_SAT_ADT_RES_DB_ENTITY_VH'
        description     = 'Search for Database entities'
        category_scheme = lv_object_search_scheme
        category_term   = 'dbentity'
    ).

    io_registry->register_discoverable_resource(
        url             = '/releasestate'
        handler_class   = 'CL_RIS_ADT_RES_RELEASE_STATES'
        description     = 'Search for Release API states'
        category_scheme = lv_object_search_scheme
        category_term   = 'releasestate'
    ).

    io_registry->register_discoverable_resource(
        url             = '/cdstype'
        handler_class   = 'ZCL_SAT_ADT_RES_CDS_TYPE_VH'
        description     = 'Search for CDS source types'
        category_scheme = lv_object_search_scheme
        category_term   = 'cdstype'
    ).

    io_registry->register_discoverable_resource(
        url             = '/cdsextension'
        handler_class   = 'ZCL_SAT_ADT_RES_CDS_EXT_VH'
        description     = 'Search for CDS Extension'
        category_scheme = lv_object_search_scheme
        category_term   = 'cdsextension'
    ).

    io_registry->register_discoverable_resource(
        url             = '/tabletype'
        handler_class   = 'ZCL_SAT_ADT_RES_TABLE_TYPE_VH'
        description     = 'Resource for Table Types'
        category_scheme = lv_object_search_scheme
        category_term   = 'tabletype'
    ).

    io_registry->register_discoverable_resource(
        url             = '/abaplanguage'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_ALANG_VH'
        description     = 'Resource for available ABAP Languages'
        category_scheme = lv_object_search_scheme
        category_term   = 'abaplanguage'
    ).

    io_registry->register_discoverable_resource(
        url             = '/classcategory'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_CATEG_VH'
        description     = 'Resource for ABAP Class Categories'
        category_scheme = lv_object_search_scheme
        category_term   = 'classcategory'
    ).

    io_registry->register_discoverable_resource(
        url             = '/classflag'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_FLAG_VH'
        description     = 'Resource for ABAP Class Flags'
        category_scheme = lv_object_search_scheme
        category_term   = 'classflag'
    ).

    io_registry->register_discoverable_resource(
        url             = '/classtype'
        handler_class   = 'ZCL_SAT_ADT_RES_CLASS_TYPE_VH'
        description     = 'Resource for Class Type'
        category_scheme = lv_object_search_scheme
        category_term   = 'classtype'
    ).

    io_registry->register_discoverable_resource(
        url             = '/dclass'
        handler_class   = 'ZCL_SAT_ADT_RES_DB_DLVCLASS_VH'
        description     = 'Resource for Delivery Classes'
        category_scheme = lv_object_search_scheme
        category_term   = 'tabledeliveryclass'
    ).

  ENDMETHOD.

  METHOD register_sapaox_launcher.
    CONSTANTS: lc_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_AOX_LAUNCHER'.

    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
        url             = c_sapaox_launcher_uri
        handler_class   = lc_handler
        description     = 'Analysis for Office Launcher'
        category_scheme = c_utils_root_scheme && c_sapaox_launcher_uri
        category_term   = 'sapaox'
    ).

*.. Build template for Analysis for Office Launcher
    DATA(lv_template) = |{ c_sapaox_launcher_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name }\}|.

*.. Register URI template for Analyis for Office launcher
    lo_element_info_collection->register_disc_res_w_template(
      template      = lv_template
      handler_class = lc_handler
      relation      = c_utils_rel_scheme && c_sapaox_launcher_uri
    ).

  ENDMETHOD.


  METHOD register_cds_analysis.
    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
        url             = c_cds_analysis_uri
        handler_class   = 'ZCL_SAT_ADT_RES_CDS_ANALYSIS'
        description     = 'Resource for CDS Analysis'
        category_scheme = c_utils_root_scheme && c_cds_analysis_uri
        category_term   = 'cdsanalysis'
    ).

*.. Register URI template for Top-Down Analysis
    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_cds_top_down_analysis_uri }\{?{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-cds_name }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-with_associations }*\}|
      handler_class = 'ZCL_SAT_ADT_RES_CDS_A_TOPDOWN'
      relation      = c_utils_rel_scheme && c_cds_top_down_analysis_uri
    ).
*.. Register URI template for Used Entities Analyis
    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_cds_used_entites_analy_uri }\{?{ zif_sat_c_adt_utils=>c_cds_analysis_parameter-cds_name }\}|
      handler_class = 'ZCL_SAT_ADT_RES_CDS_A_USED_ENT'
      relation      = c_utils_rel_scheme && c_cds_used_entites_analy_uri
    ).
  ENDMETHOD.

  METHOD register_navigation_targets.
    CONSTANTS: lc_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_NAV_TARGETS'.

    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
        url             = c_nav_targets_uri
        handler_class   = lc_handler
        description     = 'Resource for Navigation targets'
        category_scheme = c_utils_root_scheme && c_nav_targets_uri
        category_term   = 'navigationtargets'
    ).

    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_nav_targets_uri }\{?{ zif_sat_c_adt_utils=>c_element_info_parameter-name }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_element_info_parameter-object_type }\}|
      handler_class = lc_handler
      relation      = c_utils_rel_scheme && c_nav_targets_uri
    ).

  ENDMETHOD.

  METHOD register_db_field_resources.

*.. Register resource for reading the hierarchy or where-used list of Table / View / CDS Field
    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
       url             = c_column_info_uri
       handler_class   = 'ZCL_SAT_ADT_RES_COLUMN_INFO'
       description     = 'Resource for reading information about a column of an entity'
       category_scheme = c_utils_root_scheme && c_column_info_uri
       category_term   = 'column-information'
    ).

*.. Register template for column where-used list
    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_column_where_used_uri }\{?{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-name }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-field }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_calc_fields }*\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-search_db_views }*\}|
      handler_class = 'ZCL_SAT_ADT_RES_COL_WHERE_USED'
      relation      = c_utils_rel_scheme && c_column_where_used_uri
    ).
*.. Register template for column hierarchy
    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_column_hierarchy_uri }\{?{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-name }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_db_fields_info_parameter-field }\}|
      handler_class = 'ZCL_SAT_ADT_RES_COL_HIERARCHY'
      relation      = c_utils_rel_scheme && c_column_hierarchy_uri
    ).
  ENDMETHOD.


  METHOD register_ddic_repo_access.
    CONSTANTS: lc_handler TYPE string VALUE 'ZCL_SAT_ADT_RES_DDIC_REP_ACC'.
**.. Register resource for DDIC Repository access
    DATA(lo_element_info_collection) = io_registry->register_discoverable_resource(
       url             = c_db_fields_info_uri
       handler_class   = lc_handler
       description     = 'Resource for Repository DDIC Access'
       category_scheme = c_utils_root_scheme && c_ddic_repo_access_uri
       category_term   = 'ddicAccess'
    ).

    lo_element_info_collection->register_disc_res_w_template(
      template      = |{ c_ddic_repo_access_uri }\{?{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-access_mode }\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-uri }*\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-paths }*\}| &&
                      |\{&{ zif_sat_c_adt_utils=>c_ddic_repo_access_params-filters }*\}|
      handler_class = lc_handler
      relation      = c_utils_rel_scheme && c_ddic_repo_access_uri
    ).
  ENDMETHOD.

ENDCLASS.