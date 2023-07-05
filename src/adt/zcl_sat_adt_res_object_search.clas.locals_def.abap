*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_devclass_util DEFINITION.

  PUBLIC SECTION.
    METHODS determine_package_hierarchy
      IMPORTING
        search_results TYPE zif_sat_ty_object_search=>ty_t_search_result.

    METHODS get_package_uri
      IMPORTING
        package_name  TYPE devclass
      RETURNING
        VALUE(result) TYPE string.

    METHODS add_packages_to_adt_result
      CHANGING
        search_result TYPE zif_sat_ty_adt_types=>ty_s_search_result.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_package,
        devclass        TYPE devclass,
        parent_devclass TYPE devclass,
        ctext           TYPE tdevct-ctext,
        uri             TYPE string,
      END OF ty_package.

    DATA packages TYPE SORTED TABLE OF ty_package WITH UNIQUE KEY devclass.
ENDCLASS.


CLASS lcl_result_converter DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_query_result TYPE zif_sat_ty_object_search=>ty_t_search_result.

    METHODS convert
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_s_search_result.

  PROTECTED SECTION.
    DATA mt_query_result  TYPE zif_sat_ty_object_search=>ty_t_search_result.
    DATA mo_devclass_util TYPE REF TO lcl_devclass_util.

    METHODS before_conversion.

    METHODS after_conversion.

    METHODS convert_result_entry
      IMPORTING
        is_result_entry TYPE zif_sat_ty_object_search=>ty_s_search_result
      CHANGING
        cs_result       TYPE zif_sat_ty_adt_types=>ty_s_adt_obj_ref.
ENDCLASS.


CLASS lcl_cds_result_converter DEFINITION
INHERITING FROM lcl_result_converter.

  PROTECTED SECTION.
    METHODS before_conversion    REDEFINITION.
    METHODS convert_result_entry REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_ddls_source,
        ddlname TYPE ddlname,
        source  TYPE string,
      END OF ty_s_ddls_source,
      ty_lt_ddlname TYPE RANGE OF ddlname.

    DATA mt_ddls_source TYPE HASHED TABLE OF ty_s_ddls_source WITH UNIQUE KEY ddlname.

    METHODS read_ddl_sources.

    METHODS set_ddl_positional_uri
      IMPORTING
        is_result_entity TYPE zif_sat_ty_object_search=>ty_s_search_result
      CHANGING
        cs_result        TYPE zif_sat_ty_adt_types=>ty_s_adt_obj_ref.
ENDCLASS.


CLASS lcl_result_converter_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_result_converter
      IMPORTING
        iv_search_type  TYPE zif_sat_ty_object_search=>ty_search_type
        it_query_result TYPE zif_sat_ty_object_search=>ty_t_search_result
      RETURNING
        VALUE(result)   TYPE REF TO lcl_result_converter.
ENDCLASS.
