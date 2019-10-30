*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_dummy_content_handler DEFINITION.

  PUBLIC SECTION.
    INTERFACES if_adt_rest_content_handler.

    METHODS constructor
      IMPORTING
        iv_launcher_xml TYPE xstring.

  PRIVATE SECTION.
    DATA mv_launcher_xml TYPE xstring.
ENDCLASS.
