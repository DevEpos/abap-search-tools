"! <p class="shorttext synchronized">Search options for data element</p>
INTERFACE zif_sat_c_os_dtel_options
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_filter_key,
      label             TYPE string VALUE 'label',
      length            TYPE string VALUE 'length',
      table             TYPE string VALUE 'table',
      data_type         TYPE string VALUE 'dtype',
      type_category     TYPE string VALUE 'category',
      ref_type          TYPE string VALUE 'rtype',
      param             TYPE string VALUE 'param',
      shlp_name         TYPE string VALUE 'shlp',
      shlp_param        TYPE string VALUE 'shlpparam',
      default_component TYPE string VALUE 'defcomp',
      flag              TYPE string VALUE 'flag',
    END OF c_filter_key.

  CONSTANTS:
    BEGIN OF c_type_category,
      BEGIN OF int,
        ref_type   TYPE dd04l-refkind VALUE 'R',
        predefined TYPE dd04l-refkind VALUE '',
        domain     TYPE dd04l-refkind VALUE 'D',
      END OF int,
      BEGIN OF ext,
        ref_type   TYPE string VALUE 'REF_TYPE',
        predefined TYPE string VALUE 'PREDEFINED',
        domain     TYPE string VALUE 'DOMAIN',
      END OF ext,
    END OF c_type_category.

  CONSTANTS:
    BEGIN OF c_flag,
      input_history     TYPE string VALUE 'INPUT_HISTORY',
      changedoc_enabled TYPE string VALUE 'CHANGEDOC_ENABLED',
      basic_dir_is_ltr  TYPE string VALUE 'BASIC_DIR_IS_LTR',
      bidi_filtering    TYPE string VALUE 'BIDI_FILTERING',
    END OF c_flag.

  CONSTANTS:
    BEGIN OF c_ref_type,
      BEGIN OF int,
        object       TYPE dd04l-reftype VALUE 'O',
        data         TYPE dd04l-reftype VALUE 'D',
        built_in     TYPE dd04l-reftype VALUE 'B',
        data_element TYPE dd04l-reftype VALUE 'E',
        structured   TYPE dd04l-reftype VALUE 'S',
        table_type   TYPE dd04l-reftype VALUE 'L',
        class        TYPE dd04l-reftype VALUE 'C',
        interface    TYPE dd04l-reftype VALUE 'I',
      END OF int,
      BEGIN OF ext,
        object       TYPE string VALUE 'OBJECT',
        data         TYPE string VALUE 'DATA',
        built_in     TYPE string VALUE 'BUILT_IN',
        data_element TYPE string VALUE 'DATA_ELEMENT',
        structured   TYPE string VALUE 'STRUCTURED',
        table_type   TYPE string VALUE 'TABLE_TYPE',
        class        TYPE string VALUE 'CLASS',
        interface    TYPE string VALUE 'INTERFACE',
      END OF ext,
    END OF c_ref_type.
ENDINTERFACE.
