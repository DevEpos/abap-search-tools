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
        uri             TYPE string,
      END OF ty_package.

    DATA packages TYPE SORTED TABLE OF ty_package WITH UNIQUE KEY devclass.
ENDCLASS.
