"! <p class="shorttext synchronized">Utility for mapping search parameters for Class/Interface</p>
CLASS zcl_sat_clif_search_param_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized">Converts class category to external format</p>
    "!
    "! @parameter iv_internal    | <p class="shorttext synchronized">internal format</p>
    "! @parameter ev_external    | <p class="shorttext synchronized">external format</p>
    "! @parameter ev_description | <p class="shorttext synchronized">Description of the category</p>
    CLASS-METHODS convert_category_to_ext
      IMPORTING
        iv_internal           TYPE seocategry
      EXPORTING
        VALUE(ev_external)    TYPE string
        VALUE(ev_description) TYPE ddtext.

    "! <p class="shorttext synchronized">Retrieves possible class categories</p>
    "!
    "! @parameter rt_categories | <p class="shorttext synchronized"></p>
    CLASS-METHODS get_categories
      RETURNING
        VALUE(rt_categories) TYPE string_table.

    "! <p class="shorttext synchronized">Converts class category to internal format</p>
    "!
    "! @parameter iv_external | <p class="shorttext synchronized">external format</p>
    "! @parameter rv_internal | <p class="shorttext synchronized">internal format</p>
    CLASS-METHODS convert_category_to_int
      IMPORTING
        iv_external        TYPE string
      RETURNING
        VALUE(rv_internal) TYPE seocategry.

  PRIVATE SECTION.
    CLASS-DATA mt_category_values TYPE ddfixvalues.
ENDCLASS.


CLASS zcl_sat_clif_search_param_util IMPLEMENTATION.
  METHOD class_constructor.
    DATA(lo_category_type_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE seocategry( ) ) ).
    lo_category_type_descr->get_ddic_fixed_values( RECEIVING  p_fixed_values = mt_category_values
                                                   EXCEPTIONS not_found      = 1
                                                              no_ddic_type   = 2
                                                              OTHERS         = 3 ).
  ENDMETHOD.

  METHOD convert_category_to_ext.
    DATA lv_default_text TYPE ddtext.

    CLEAR ev_external.

    CASE iv_internal.

      WHEN '00'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-general.
        lv_default_text = 'General Class'.

      WHEN '01'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-exit.
        lv_default_text = 'Exit Class'.

      WHEN '10'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-persistent.
        lv_default_text = 'Persistent Class'.

      WHEN '11'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-pers_factory.
        lv_default_text = 'Factory for persistent class'.

      WHEN '40'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-exception.
        lv_default_text = 'Exception Class'.

      WHEN '05'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-test_class.
        lv_default_text = 'Test Class (ABAP Unit)'.

      WHEN '45'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-area_class.
        lv_default_text = 'Area Class (Shared Objects)'.

      WHEN '80'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-wd_runtime.
        lv_default_text = 'Web Dynpro Runtime Object'.

      WHEN '06'.
        ev_external = zif_sat_c_os_clif_options=>c_class_categories-behavior.
        lv_default_text = 'Behavior (Class ... for Behavior of ...)'.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_sat_conversion_exc.
    ENDCASE.

    IF ev_external IS INITIAL.
      RETURN.
    ENDIF.
    ev_description = VALUE #( mt_category_values[ low = iv_internal ]-ddtext DEFAULT lv_default_text  ).
  ENDMETHOD.

  METHOD convert_category_to_int.
    CASE iv_external.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-general.
        rv_internal = '00'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-exit.
        rv_internal = '01'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-persistent.
        rv_internal = '10'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-pers_factory.
        rv_internal = '11'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-exception.
        rv_internal = '40'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-test_class.
        rv_internal = '05'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-area_class.
        rv_internal = '45'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-wd_runtime.
        rv_internal = '80'.

      WHEN zif_sat_c_os_clif_options=>c_class_categories-behavior.
        rv_internal = '06'.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_sat_conversion_exc.
    ENDCASE.
  ENDMETHOD.

  METHOD get_categories.
    rt_categories = VALUE #( FOR cat IN mt_category_values
                             ( |{ cat-low }| ) ).
  ENDMETHOD.
ENDCLASS.
