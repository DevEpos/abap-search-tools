"! <p class="shorttext synchronized">Options/Constants for Class/Interface Search</p>
INTERFACE zif_sat_c_os_clif_options
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Search options for class/interface object search</p>
    BEGIN OF c_filter_key,
      attribute  TYPE string VALUE 'attr',
      method     TYPE string VALUE 'meth',
      super_type TYPE string VALUE 'super',
      interface  TYPE string VALUE 'intf',
      "! Category of the class/interface (see data element SEOCATEGRY)
      "! --> check against current NW stack to get valid options
      category   TYPE string VALUE 'cat',
      friend     TYPE string VALUE 'friend',
      "! Checks for the following criteria
      "! <ul>
      "!   <li>abstract</li>
      "!   <li>final</li>
      "!   <li>test</li>
      "!   <li>fixpoint</li>
      "!   <li>shared_memory</li>
      "! </ul>
      flag       TYPE string VALUE 'flag',
      "! Holds the name of referenced cds view for behavior classes
      "! (necessary???)
      ref_object TYPE string VALUE 'refObject',
    END OF c_filter_key.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Flags for Class/Interface</p>
    BEGIN OF c_class_intf_flags,
      is_abstract        TYPE string VALUE 'ABSTRACT',
      is_final           TYPE string VALUE 'FINAL',
      has_test           TYPE string VALUE 'TEST',
      is_fixpoint        TYPE string VALUE 'FIXPOINT',
      is_shared_memory   TYPE string VALUE 'SHARED_MEMORY',
      has_unicode_checks TYPE string VALUE 'UNICODE',
    END OF c_class_intf_flags.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Categories of ABAP OO Class</p>
    BEGIN OF c_class_categories,
      general      TYPE string VALUE 'GENERAL',
      exit         TYPE string VALUE 'EXIT',
      persistent   TYPE string VALUE 'PERSISTENT',
      pers_factory TYPE string VALUE 'PERS_FACTORY',
      exception    TYPE string VALUE 'EXCEPTION',
      test_class   TYPE string VALUE 'TEST_CLASS',
      area_class   TYPE string VALUE 'AREA_CLASS',
      wd_runtime   TYPE string VALUE 'WD_RUNTIME',
      behavior     TYPE string VALUE 'BEHAVIOR',
    END OF c_class_categories.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Types for class/interface</p>
    BEGIN OF c_class_types,
      class     TYPE string VALUE 'CLAS',
      interface TYPE string VALUE 'INTF',
    END OF c_class_types.

  CONSTANTS:
    BEGIN OF c_custom_options,
      BEGIN OF mode_for_intf_super_filter,
        name TYPE string VALUE 'modeForIntfOrSuperFilter',
        BEGIN OF options,
          resolve_intf  TYPE string VALUE 'resolveIntf',
          resolve_super TYPE string VALUE 'resolveSuper',
        END OF options,
      END OF mode_for_intf_super_filter,
    END OF c_custom_options.
ENDINTERFACE.
