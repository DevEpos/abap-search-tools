"! <p class="shorttext synchronized">IoC Container for Object search</p>
CLASS zcl_sat_search_ioc DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_sat_base_ioc
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_c_object_search.

    ALIASES c_search_type FOR zif_sat_c_object_search~c_search_type.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_contracts,
        query_validator TYPE classname VALUE 'ZIF_SAT_QUERY_VALIDATOR',
        query_converter TYPE classname VALUE 'ZIF_SAT_QUERY_CONVERTER',
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
    )->add_implementer( iv_filter      = |{ c_search_type-cds_view }|
                        iv_implementer = 'ZCL_SAT_CDS_VIEW_QUERY_CONFIG'
    )->add_implementer( iv_filter      = |{ c_search_type-db_tab }|
                        iv_implementer = 'ZCL_SAT_DBTAB_QUERY_CONFIG'
    )->add_implementer( iv_filter      = |{ c_search_type-ddic_view }|
                        iv_implementer = 'ZCL_SAT_DDICVIEW_QUERY_CONFIG'
    )->add_implementer( iv_filter      = |{ c_search_type-class_interface }|
                        iv_implementer = 'ZCL_SAT_CLSINTF_QUERY_CONFIG'
    )->add_implementer( iv_filter      = |{ c_search_type-method }|
                        iv_implementer = 'ZCL_SAT_CLIF_METH_QUERY_CONFIG'
    )->add_implementer( iv_filter      = |{ c_search_type-message }|
                        iv_implementer = 'ZCL_SAT_MESSAGE_QUERY_CONFIG' ).

    add_contract( iv_contract = c_contracts-query_validator
    )->add_implementer( iv_filter      = |{ c_search_type-cds_view }|
                        iv_implementer = 'ZCL_SAT_CDS_VIEW_QV'
    )->add_implementer( iv_filter      = |{ c_search_type-db_tab }|
                        iv_implementer = 'ZCL_SAT_DBTAB_QV'
    )->add_implementer( iv_filter      = |{ c_search_type-class_interface }|
                        iv_implementer = 'ZCL_SAT_CLSINTF_QV'
    )->add_implementer( iv_filter      = |{ c_search_type-method }|
                        iv_implementer = 'ZCL_SAT_CLIF_METHOD_QV'
    )->add_implementer( iv_implementer = 'ZCL_SAT_GENERAL_QV' ).

    add_contract( iv_contract = c_contracts-search_provider
    )->add_implementer( iv_filter      = |{ c_search_type-cds_view }|
                        iv_implementer = 'ZCL_SAT_OS_CDS_PROVIDER'
    )->add_implementer( iv_filter      = |{ c_search_type-db_tab }|
                        iv_implementer = 'ZCL_SAT_OS_DBTAB_PROVIDER'
    )->add_implementer( iv_filter      = |{ c_search_type-ddic_view }|
                        iv_implementer = 'ZCL_SAT_OS_DDICVIEW_PROVIDER'
    )->add_implementer( iv_filter      = |{ c_search_type-class_interface }|
                        iv_implementer = 'ZCL_SAT_OS_CLASSINTF_PROVIDER'
    )->add_implementer( iv_filter      = |{ c_search_type-method }|
                        iv_implementer = 'ZCL_SAT_OS_METHOD_PROVIDER'
    )->add_implementer( iv_filter      = |{ c_search_type-message }|
                        iv_implementer = 'ZCL_SAT_OS_MESSAGE_PROVIDER' ).

    add_contract( iv_contract = c_contracts-query_converter
    )->add_implementer( iv_filter      = |{ c_search_type-class_interface }|
                        iv_implementer = 'ZCL_SAT_CLSINTF_QC'
    )->add_implementer( iv_filter      = |{ c_search_type-method }|
                        iv_implementer = 'ZCL_SAT_CLIF_METHOD_QC'
    )->add_implementer( iv_filter      = |{ c_search_type-cds_view }|
                        iv_implementer = 'ZCL_SAT_CDS_VIEW_QC'
    )->add_implementer( iv_filter      = |{ c_search_type-db_tab }|
                        iv_implementer = 'ZCL_SAT_DBTAB_QC'
    )->add_implementer( iv_filter      = |{ c_search_type-ddic_view }|
                        iv_implementer = 'ZCL_SAT_DDICVIEW_QC'
    )->add_implementer( iv_implementer = 'ZCL_SAT_GENERAL_QC' ).

    add_contract( iv_contract = c_contracts-query_parser
    )->add_implementer( iv_filter       = |{ c_search_type-cds_view }|
                        iv_implementer  = c_implementer-query_parser
                        it_dependencies = VALUE #(
                            filter = c_search_type-cds_view
                            ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config )
                            ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                            ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    )->add_implementer( iv_filter       = |{ c_search_type-db_tab }|
                        iv_implementer  = c_implementer-query_parser
                        it_dependencies = VALUE #(
                            filter = c_search_type-db_tab
                            ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    )
                            ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                            ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    )->add_implementer( iv_filter       = |{ c_search_type-ddic_view }|
                        iv_implementer  = c_implementer-query_parser
                        it_dependencies = VALUE #(
                            filter = c_search_type-ddic_view
                            ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    )
                            ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                            ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    )->add_implementer( iv_filter       = |{ c_search_type-class_interface }|
                        iv_implementer  = c_implementer-query_parser
                        it_dependencies = VALUE #(
                            filter = c_search_type-class_interface
                            ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config )
                            ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                            ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    )->add_implementer( iv_filter       = |{ c_search_type-method }|
                        iv_implementer  = c_implementer-query_parser
                        it_dependencies = VALUE #(
                            filter = c_search_type-method
                            ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config )
                            ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                            ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    )->add_implementer( iv_filter       = |{ c_search_type-message }|
                        iv_implementer  = c_implementer-query_parser
                        it_dependencies = VALUE #(
                            filter = c_search_type-message
                            ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config )
                            ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                            ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) ) ).
  ENDMETHOD.
ENDCLASS.
