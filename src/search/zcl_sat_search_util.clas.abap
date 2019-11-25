"! <p class="shorttext synchronized" lang="en">Utils for Object search</p>
CLASS zcl_sat_search_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Removes the exclusion strings from the given value</p>
    "!
    "! @parameter cv_value | <p class="shorttext synchronized" lang="en">Value with possible exc</p>
    "! @parameter cv_sign | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS remove_exclusion_string
      CHANGING
        cv_value TYPE string
        cv_sign  TYPE ddsign OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_search_util IMPLEMENTATION.

  METHOD remove_exclusion_string.
    IF cv_value CP '!*'.
      cv_value = cv_value+1.
      cv_sign = zif_sat_c_options=>excluding.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
