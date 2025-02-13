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
      END OF c_implementer,
      BEGIN OF c_dependency_params,
        io_configuration TYPE string VALUE 'IO_CONFIGURATION',
        io_validator     TYPE string VALUE 'IO_VALIDATOR',
        io_converter     TYPE string VALUE 'IO_CONVERTER',
      END OF c_dependency_params.

ENDCLASS.


CLASS zcl_sat_search_ioc IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    add_contracts(
        VALUE #(
            ( name = 'ZIF_SAT_SEARCH_ENGINE' implementers = VALUE #( ( implementer = 'ZCL_SAT_SEARCH_ENGINE' ) ) )
            ( name         = c_contracts-query_config
              implementers = VALUE #( ( filter      = |{ c_search_type-cds_view }|
                                        implementer = 'ZCL_SAT_CDS_VIEW_QUERY_CONFIG' )
                                      ( filter      = |{ c_search_type-db_tab }|
                                        implementer = 'ZCL_SAT_DBTAB_QUERY_CONFIG' )
                                      ( filter      = |{ c_search_type-structure }|
                                        implementer = 'ZCL_SAT_STRUCT_QUERY_CONFIG' )
                                      ( filter      = |{ c_search_type-ddic_view }|
                                        implementer = 'ZCL_SAT_DDICVIEW_QUERY_CONFIG' )
                                      ( filter      = |{ c_search_type-class_interface }|
                                        implementer = 'ZCL_SAT_CLSINTF_QUERY_CONFIG' )
                                      ( filter      = |{ c_search_type-method }|
                                        implementer = 'ZCL_SAT_CLIF_METH_QUERY_CONFIG' )
                                      ( filter      = |{ c_search_type-message }|
                                        implementer = 'ZCL_SAT_MESSAGE_QUERY_CONFIG' ) ) )
            ( name         = c_contracts-query_validator
              implementers = VALUE #( ( filter      = |{ c_search_type-cds_view }|
                                        implementer = 'ZCL_SAT_CDS_VIEW_QV' )
                                      ( filter      = |{ c_search_type-db_tab }|
                                        implementer = 'ZCL_SAT_DBTAB_QV' )
                                      ( filter      = |{ c_search_type-class_interface }|
                                        implementer = 'ZCL_SAT_CLSINTF_QV' )
                                      ( filter      = |{ c_search_type-method }|
                                        implementer = 'ZCL_SAT_CLIF_METHOD_QV' )
                                      ( implementer = 'ZCL_SAT_GENERAL_QV' ) ) )
            ( name         = c_contracts-search_provider
              implementers = VALUE #( ( filter      = |{ c_search_type-cds_view }|
                                        implementer = 'ZCL_SAT_OS_CDS_PROVIDER' )
                                      ( filter      = |{ c_search_type-db_tab }|
                                        implementer = 'ZCL_SAT_OS_DBTAB_PROVIDER' )
                                      ( filter      = |{ c_search_type-structure }|
                                        implementer = 'ZCL_SAT_OS_STRUCT_PROVIDER' )
                                      ( filter      = |{ c_search_type-ddic_view }|
                                        implementer = 'ZCL_SAT_OS_DDICVIEW_PROVIDER' )
                                      ( filter      = |{ c_search_type-class_interface }|
                                        implementer = 'ZCL_SAT_OS_CLASSINTF_PROVIDER' )
                                      ( filter      = |{ c_search_type-method }|
                                        implementer = 'ZCL_SAT_OS_METHOD_PROVIDER' )
                                      ( filter      = |{ c_search_type-message }|
                                        implementer = 'ZCL_SAT_OS_MESSAGE_PROVIDER' ) ) )
            ( name         = c_contracts-query_converter
              implementers = VALUE #( ( filter      = |{ c_search_type-class_interface }|
                                        implementer = 'ZCL_SAT_CLSINTF_QC' )
                                      ( filter      = |{ c_search_type-method }|
                                        implementer = 'ZCL_SAT_CLIF_METHOD_QC' )
                                      ( filter      = |{ c_search_type-cds_view }|
                                        implementer = 'ZCL_SAT_CDS_VIEW_QC' )
                                      ( filter      = |{ c_search_type-db_tab }|
                                        implementer = 'ZCL_SAT_DBTAB_QC' )
                                      ( filter      = |{ c_search_type-ddic_view }|
                                        implementer = 'ZCL_SAT_DDICVIEW_QC' )
                                      ( implementer = 'ZCL_SAT_GENERAL_QC' ) ) )
            ( name         = c_contracts-query_parser
              implementers = VALUE #(
                  implementer = c_implementer-query_parser
                  ( filter       = |{ c_search_type-cds_view }|
                    dependencies = VALUE #(
                        filter = c_search_type-cds_view
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) )
                  ( filter       = |{ c_search_type-db_tab }|
                    dependencies = VALUE #(
                        filter = c_search_type-db_tab
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config    )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) )
                  ( filter       = |{ c_search_type-structure }|
                    dependencies = VALUE #(
                        filter = c_search_type-structure
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config    )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) )
                  ( filter       = |{ c_search_type-ddic_view }|
                    dependencies = VALUE #(
                        filter = c_search_type-ddic_view
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config    )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) )
                  ( filter       = |{ c_search_type-class_interface }|
                    dependencies = VALUE #(
                        filter = c_search_type-class_interface
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) )
                  ( filter       = |{ c_search_type-method }|
                    dependencies = VALUE #(
                        filter = c_search_type-method
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) )
                  ( filter       = |{ c_search_type-message }|
                    dependencies = VALUE #(
                        filter = c_search_type-message
                        ( parameter = c_dependency_params-io_configuration contract = c_contracts-query_config )
                        ( parameter = c_dependency_params-io_validator     contract = c_contracts-query_validator )
                        ( parameter = c_dependency_params-io_converter     contract = c_contracts-query_converter ) ) ) ) ) ) ).
  ENDMETHOD.
ENDCLASS.
