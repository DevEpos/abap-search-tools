"! <p class="shorttext synchronized" lang="en">IoC Container for Object search</p>
CLASS zcl_sat_search_ioc DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_sat_base_ioc
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_c_object_search.

    ALIASES:
      c_search_type FOR zif_sat_c_object_search~c_search_type.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_contracts,
        query_validator TYPE classname VALUE 'ZIF_SAT_QUERY_VALIDATOR',
        query_parser    TYPE classname VALUE 'ZIF_SAT_OBJECT_QUERY_PARSER',
        query_config    TYPE classname VALUE 'ZIF_SAT_OBJECT_SEARCH_CONFIG',
        search_provider TYPE classname VALUE 'ZIF_SAT_OBJECT_SEARCH_PROVIDER',
        search_engine   TYPE classname VALUE 'ZIF_SAT_SEARCH_ENGINE',
      END OF c_contracts,
      BEGIN OF c_implementer,
        query_parser TYPE classname VALUE 'ZCL_SAT_OBJECT_QUERY_PARSER',
      END OF c_implementer.

ENDCLASS.



CLASS zcl_sat_search_ioc IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    add_contract( iv_contract = 'ZIF_SAT_SEARCH_ENGINE' )->add_implementer( iv_implementer = 'ZCL_SAT_SEARCH_ENGINE' ).

    add_contract( iv_contract = c_contracts-query_config
    )->add_implementer( iv_filter = |{ c_search_type-cds_view }|        iv_implementer = 'ZCL_SAT_CDS_VIEW_QUERY_CONFIG'
    )->add_implementer( iv_filter = |{ c_search_type-db_tab_view }|     iv_implementer = 'ZCL_SAT_DBTABVIEW_QUERY_CONFIG'
    )->add_implementer( iv_filter = |{ c_search_type-class_interface }| iv_implementer = 'ZCL_SAT_CLSINTF_QUERY_CONFIG' ).

    add_contract( iv_contract = c_contracts-query_validator
    )->add_implementer( iv_filter = |{ c_search_type-cds_view }|    iv_implementer = 'ZCL_SAT_CDS_VIEW_QV'
    )->add_implementer( iv_filter = |{ c_search_type-db_tab_view }| iv_implementer = 'ZCL_SAT_DBTABVIEW_QV'
    )->add_implementer( iv_implementer = 'ZCL_SAT_GENERAL_QV' ).

    add_contract( iv_contract = c_contracts-search_provider
    )->add_implementer( iv_filter = |{ c_search_type-cds_view }|        iv_implementer = 'ZCL_SAT_OS_CDS_PROVIDER'
    )->add_implementer( iv_filter = |{ c_search_type-db_tab_view }|     iv_implementer = 'ZCL_SAT_OS_DBTAB_PROVIDER'
    )->add_implementer( iv_filter = |{ c_search_type-class_interface }| iv_implementer = 'ZCL_SAT_OS_CLASSINTF_PROVIDER' ).

    add_contract( iv_contract = c_contracts-query_parser
    )->add_implementer( iv_filter      = |{ c_search_type-cds_view }|
                        iv_implementer = c_implementer-query_parser
                        it_dependencies = VALUE #(
                         ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    filter = c_search_type-cds_view )
                         ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator filter = c_search_type-cds_view ) )
    )->add_implementer( iv_filter      = |{ c_search_type-db_tab_view }|
                        iv_implementer = c_implementer-query_parser
                        it_dependencies = VALUE #(
                         ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    filter = c_search_type-db_tab_view )
                         ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator filter = c_search_type-db_tab_view ) )
    )->add_implementer( iv_filter      = |{ c_search_type-class_interface }|
                        iv_implementer = c_implementer-query_parser
                        it_dependencies = VALUE #(
                         ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    filter = c_search_type-class_interface )
                         ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator ) )
    ).
  ENDMETHOD.

ENDCLASS.
