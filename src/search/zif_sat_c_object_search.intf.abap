"! <p class="shorttext synchronized">Constants for Object search</p>
INTERFACE zif_sat_c_object_search
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_search_type,
      cds_view        TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'cds',
      db_tab          TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'dbtab',
      ddic_view       TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'ddicview',
      class_interface TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'classintf',
      method          TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'method',
      db_field        TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'dbfield',
      cds_field       TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'cdsfield',
      message         TYPE zif_sat_ty_object_search=>ty_search_type VALUE 'message',
    END OF c_search_type.

  CONSTANTS:
    BEGIN OF c_search_fields,
      object_name_input_key     TYPE string VALUE 'objectName',
      object_name_input_label   TYPE string VALUE 'Object &Name',
      object_filter_input_key   TYPE string VALUE 'objectFilter',
      object_filter_input_label TYPE string VALUE 'Object &Filters',
    END OF c_search_fields.

  CONSTANTS:
    BEGIN OF c_image_registry_id,
      adt_type TYPE string VALUE 'ADT_OBJECT_TYPE',
    END OF c_image_registry_id.

  CONSTANTS:
    BEGIN OF c_proposal_image_source,
      same_as_filter TYPE string VALUE 'SAME_AS_FILTER',
      proposal       TYPE string VALUE 'PROPOSAL',
    END OF c_proposal_image_source.

  CONSTANTS c_image_id_prefix TYPE string VALUE 'imageId='.

  CONSTANTS:
    BEGIN OF c_filter_data_type,
      default TYPE string VALUE '',
      date    TYPE string VALUE 'DATE',
      boolean TYPE string VALUE 'BOOLEAN',
    END OF c_filter_data_type.

  CONSTANTS:
    BEGIN OF c_custom_option_data_type,
      string  TYPE string VALUE 'STRING',
      boolean TYPE string VALUE 'BOOLEAN',
      combo   TYPE string VALUE 'COMBO',
    END OF c_custom_option_data_type.

  CONSTANTS:
    BEGIN OF c_filter_content_assist_type,
      named_item       TYPE string VALUE 'objectsearch:NamedItemContentAssist',
      fixed_named_item TYPE string VALUE 'objectsearch:FixedValuesContentAssist',
      ris              TYPE string VALUE 'objectsearch:RisContentAssist',
      user             TYPE string VALUE 'objectsearch:UserContentAssist',
    END OF c_filter_content_assist_type.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">General search options</p>
    BEGIN OF c_general_search_params,
      object_name            TYPE string VALUE 'objectName',
      object_type            TYPE string VALUE 'objectType',
      max_rows               TYPE string VALUE 'maxrows',
      user                   TYPE string VALUE 'owner',
      changed_by             TYPE string VALUE 'changedby',
      application_component  TYPE string VALUE 'appl',
      software_component     TYPE string VALUE 'comp',
      release_state          TYPE string VALUE 'api',
      description            TYPE string VALUE 'desc',
      type                   TYPE string VALUE 'type',
      package                TYPE string VALUE 'package',
      created_on             TYPE string VALUE 'created',
      changed_on             TYPE string VALUE 'changed',
      maintenance            TYPE string VALUE 'maintflag',
      use_and_for_filters    TYPE string VALUE 'useAndForFilters',
      read_api_state         TYPE string VALUE 'withApiState',
      get_all_results        TYPE string VALUE 'getAllResults',
      read_package_hierarchy TYPE string VALUE 'withPackageHierarchy',
    END OF c_general_search_params.

  CONSTANTS:
    "! Values for API option
    BEGIN OF c_api_option_value,
      released   TYPE string VALUE 'RELEASED',
      deprecated TYPE string VALUE 'DEPRECATED',
    END OF c_api_option_value.

  CONSTANTS:
    "! Constants for value help providers for search parameters in
    "! search input fields
    BEGIN OF c_content_assist,
      category_scheme TYPE string VALUE 'http://www.devepos.com/adt/saat/v2/objectsearch',
      BEGIN OF terms,
        cds_field         TYPE string VALUE 'cdsfield',
        cds_base_field    TYPE string VALUE 'cdsbasefield',
        cds_param         TYPE string VALUE 'cdsparam',
        appl_comp         TYPE string VALUE 'applcomp',
        software_comp     TYPE string VALUE 'softcomp',
        table_field       TYPE string VALUE 'tablefield',
        annotation        TYPE string VALUE 'annotation',
        annotatio_value   TYPE string VALUE 'annotationvalue',
        db_entity         TYPE string VALUE 'dbentity',
        release_state     TYPE string VALUE 'releasestate',
        cds_type          TYPE string VALUE 'cdstype',
        cds_extension     TYPE string VALUE 'cdsextension',
        class_category    TYPE string VALUE 'classcategory',
        class_flag        TYPE string VALUE 'classflag',
        class_type        TYPE string VALUE 'classtype',
        table_deliv_class TYPE string VALUE 'tabledeliveryclass',
        table_data_class  TYPE string VALUE 'tabledataclass',
        view_field        TYPE string VALUE 'viewfield',
        view_root_tab     TYPE string VALUE 'viewroottable',
        view_base_tab     TYPE string VALUE 'viewbasetable',
        db_tab_include    TYPE string VALUE 'dbtableinclude',
      END OF terms,
    END OF c_content_assist.
ENDINTERFACE.
