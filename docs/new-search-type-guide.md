# Guide to implement a new Search Type

## Create Configuration Class

The configuration class is the most important class next to the actual search provider class. It specifies the available search fields, the custom options.

The class must implement the interface [ZIF_SAT_OBJECT_SEARCH_CONFIG](../src/search/zif_sat_object_search_config.intf.abap), but it's best to inherit from the class [ZCL_SAT_BASE_QUERY_CONFIG](../src/search/zcl_sat_base_query_config.clas.abap) which includes methods for some standard filters that old search types should declare.

## Create Validation Class (Optional)

Depending on the types of filters the new search type should support you may have to provide custom validation logic. This custom validation logic class must implement the interface [ZIF_SAT_QUERY_VALIDATOR](../src/search/zif_sat_query_validator.intf.abap).

It's best to inherit from the class [ZCL_SAT_GENERAL_QV](../src/search/zcl_sat_general_qv.clas.abap) to let it handle some common filter validations.

## Create Converter Class (Optional)

Sometime, you might consider offering different filter choices in the user interface to enhance clarity on how the filter choice affects the query result. However this filter value would then have to be converted before it can be applied in an SQL query.

To do this you have to create a new class which implements the interface [ZIF_SAT_QUERY_CONVERTER](../src/search/zif_sat_query_converter.intf.abap), but again it's best to inherit from the class [ZCL_SAT_GENERAL_QC](../src/search/zcl_sat_general_qc.clas.abap) to let it handle some common filter value conversions.

## Create Search Provider Class


## Register the Search Configuration for ADT

## Create/Register Converter for Query Results (Optional)

## Register the classes in the Search IoC Container

