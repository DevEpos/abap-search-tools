"! <p class="shorttext synchronized">Base Search provider for Methods</p>
CLASS zcl_sat_os_subp_meth_base DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_sat_os_classintf_provider
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_method_key_reader ABSTRACT METHODS get_method_key.

  PROTECTED SECTION.
    ALIASES c_class_intf_search_option FOR zif_sat_c_object_search~c_class_intf_search_option.
    ALIASES c_method_option            FOR zif_sat_c_object_search~c_method_search_option.
    ALIASES get_method_key             FOR zif_sat_method_key_reader~get_method_key.

    TYPES:
      BEGIN OF ty_method,
        clsname      TYPE seoclsname,
        parent_class TYPE seoclsname,
        cmpname      TYPE seocmpname,
        exposure     TYPE seoexpose,
        final        TYPE abap_bool,
        created_by   TYPE uname,
        created_on   TYPE dats,
        changed_by   TYPE uname,
        changed_on   TYPE dats,
      END OF ty_method,

      BEGIN OF ty_class,
        developmentpackage TYPE zsat_i_classinterface-developmentpackage,
        tadirtype          TYPE zsat_i_classinterface-tadirtype,
      END OF ty_class,

      BEGIN OF ty_include,
        progname TYPE reposrc-progname,
      END OF ty_include.

    DATA mt_meth_final_filter TYPE RANGE OF abap_bool.
    DATA mt_meth_cls_exc_filter TYPE RANGE OF abap_bool.
    DATA mt_meth_level_filter TYPE RANGE OF seomtddecl.
    DATA mt_meth_type_filter TYPE RANGE OF seomtdtype.
    DATA mt_meth_status_filter TYPE RANGE OF string.
    DATA mt_meth_exposure_filter TYPE RANGE OF seoexpose.
    DATA mt_meth_name_filter TYPE RANGE OF string.
    DATA mt_meth_param_filter TYPE RANGE OF seosconame.
    DATA mt_meth_exc_filter TYPE RANGE OF seosconame.

    METHODS read_method_infos_n_filter.
    METHODS set_method_filters.

    METHODS method_matches_filter
      IMPORTING
        iv_method_name TYPE seocmpname
        is_method      TYPE zif_sat_ty_object_search=>ty_s_search_result
        is_method_info TYPE vseomethod
      RETURNING
        VALUE(result)  TYPE abap_bool.

    METHODS set_type_filter_to_class.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_os_subp_meth_base IMPLEMENTATION.
  METHOD read_method_infos_n_filter.
    DATA ls_method_info TYPE vseomethod.
    DATA ls_method_details TYPE seoo_method_details.

    LOOP AT mt_result REFERENCE INTO DATA(lr_class_method).
      DATA(ls_mtdkey) = get_method_key( iv_classname   = lr_class_method->object_name
                                        iv_method_name = lr_class_method->method_name ).
      IF ls_mtdkey IS INITIAL.
        DELETE mt_result.
        CONTINUE.
      ENDIF.

      CLEAR: ls_method_info,
             ls_method_details.

      CALL FUNCTION 'SEO_METHOD_GET_DETAIL'
        EXPORTING  cpdkey         = ls_mtdkey
        IMPORTING  method         = ls_method_info
                   method_details = ls_method_details
        EXCEPTIONS not_existing   = 1
                   no_method      = 2
                   OTHERS         = 3.

      IF sy-subrc <> 0.
        DELETE mt_result.
        CONTINUE.
      ENDIF.

      IF ls_method_info IS INITIAL.
        DELETE mt_result.
        CONTINUE.
      ENDIF.

      lr_class_method->method_descr       = ls_method_info-descript.
      lr_class_method->method_exposure    = ls_method_info-exposure.
      lr_class_method->method_name        = ls_mtdkey-cpdname.
      lr_class_method->method_decl_clif   = ls_method_info-clsname.
      lr_class_method->method_decl_method = ls_method_info-cmpname.
      lr_class_method->method_is_final    = ls_method_info-mtdfinal.
      lr_class_method->method_type        = ls_method_info-mtdtype.
      lr_class_method->method_level       = ls_method_info-mtddecltyp.
      lr_class_method->method_status      = COND #(
        WHEN ls_method_details-is_final_redefined          = abap_true
          OR ls_method_details-is_redefined                = abap_true
          OR ls_method_details-is_final_redefined_in_super = abap_true THEN
          zif_sat_c_object_search=>c_method_status_int-redefined
        WHEN ls_method_details-is_abstract_implemented  = abap_true
          OR ls_method_details-is_final_implemented     = abap_true
          OR ls_method_details-is_implemented_if_method = abap_true THEN
          zif_sat_c_object_search=>c_method_status_int-implemented
        ELSE
          zif_sat_c_object_search=>c_method_status_int-standard ).

      IF NOT method_matches_filter( iv_method_name = ls_method_info-cmpname
                                    is_method      = lr_class_method->*
                                    is_method_info = ls_method_info ).
        DELETE mt_result.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_method_filters.
    LOOP AT mo_search_query->mt_search_options REFERENCE INTO DATA(lr_filter)
         WHERE target = zif_sat_c_object_search=>c_search_fields-method_filter_input_key.

      CASE lr_filter->option.

        WHEN c_method_option-level.
          mt_meth_level_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-visibility.
          mt_meth_exposure_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_general_search_options-type.
          mt_meth_type_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-status.
          mt_meth_status_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-exception.
          mt_meth_exc_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-param.
          mt_meth_param_filter = CORRESPONDING #( lr_filter->value_range ).

        WHEN c_method_option-flag.
          LOOP AT lr_filter->value_range INTO DATA(ls_option).
            CASE ls_option-low.
              WHEN zif_sat_c_object_search=>c_method_flags-final.
                mt_meth_final_filter = VALUE #( ( sign = ls_option-sign option = ls_option-option low = abap_true ) ).
              WHEN zif_sat_c_object_search=>c_method_flags-class_exceptions.
                mt_meth_cls_exc_filter = VALUE #( ( sign = ls_option-sign option = ls_option-option low = abap_true ) ).

            ENDCASE.
          ENDLOOP.

      ENDCASE.
    ENDLOOP.

    mt_meth_name_filter = VALUE #( mo_search_query->mt_search_term[
                                       target = zif_sat_c_object_search=>c_search_fields-method_name_input_key ]-values OPTIONAL ).
  ENDMETHOD.

  METHOD method_matches_filter.
    IF     is_method-method_exposure IN mt_meth_exposure_filter
       AND is_method-method_level    IN mt_meth_level_filter
       AND is_method-method_is_final IN mt_meth_final_filter
       AND iv_method_name            IN mt_meth_name_filter
       AND is_method-method_type     IN mt_meth_type_filter
       AND is_method-method_status   IN mt_meth_status_filter
       AND is_method_info-mtdnewexc  IN mt_meth_cls_exc_filter.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set_type_filter_to_class.
    mo_search_query->set_option(
        VALUE #( target      = zif_sat_c_object_search=>c_search_fields-object_filter_input_key
                 option      = c_general_search_options-type
                 value_range = VALUE #( ( sign = 'I' option = 'EQ' low = zif_sat_c_tadir_types=>class ) ) ) ).
  ENDMETHOD.
ENDCLASS.
