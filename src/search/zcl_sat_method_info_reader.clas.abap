"! <p class="shorttext synchronized">Method info/detail reader</p>
"! <p><strong>Processing Logic</strong><br/></p>
"! <ol>
"!   <li>Case of redefined method search
"!     <ol>
"!       <li>Interface methods -> redefined -> end of processing</li>
"!       <li>Normal methods -> redefined
"!         <ol>
"!           <li>find next super class -> check if method is defined in that class -> repeat until found</li>
"!         </ol>
"!       </li>
"!    </ol>
"!   </li>
"!   <li>Case of method search via include admin data
"!     <ol>
"!       <li>Interface methods
"!         <ol>
"!           <li>check if class implements interface -> implemented -> end of processing</li>
"!           <li>if not implemented by class -> redefined + implemented -> end of processing</li>
"!         </ol>
"!       </li>
"!       <li>Normal methods
"!         <ol>
"!           <li>check if method exists in class -> normal method -> end of processing</li>
"!           <li>if not in class -> set redefined
"!             <ol>
"!               <li>find next super class -> check if method in class -> repeat until method definition found</li>
"!             </ol>
"!           </li>
"!         </ol>
"!       </li>
"!     </ol>
"!   </li>
"! </ol>
"!
CLASS zcl_sat_method_info_reader DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_results           TYPE REF TO zif_sat_ty_object_search=>ty_t_search_result
        io_method_key_reader TYPE REF TO zif_sat_method_key_reader
        if_only_redefined    TYPE abap_bool OPTIONAL.

    METHODS apply.

  PRIVATE SECTION.
    TYPES ty_status TYPE c LENGTH 1.
    TYPES ty_method_type TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_status_flags,
        look_in_current_cls    TYPE ty_status VALUE '1',
        look_in_current_parent TYPE ty_status VALUE '2',
        find_next_parent       TYPE ty_status VALUE '3',
      END OF c_status_flags,

      BEGIN OF c_method_types,
        normal      TYPE ty_method_type VALUE '1',
        implemented TYPE ty_method_type VALUE '2',
      END OF c_method_types.

    TYPES:
      BEGIN OF ty_method_info,
        status               TYPE ty_status,
        type                 TYPE ty_method_type,
        original_class_name  TYPE classname,
        original_method_name TYPE seocpdname,
        current_class        TYPE classname,
        classname            TYPE classname,
        method_name          TYPE seocmpname,
        super_class          TYPE classname,
        is_redefined         TYPE abap_bool,
        result_ref           TYPE REF TO zif_sat_ty_object_search=>ty_s_search_result,
      END OF ty_method_info,

      BEGIN OF ty_method_detail,
        clsname   TYPE seocompodf-clsname,
        cmpname   TYPE seocompodf-cmpname,
        exposure  TYPE seocompodf-exposure,
        level     TYPE seocompodf-mtddecltyp,
        is_final  TYPE seocompodf-mtdfinal,
        descript  TYPE seocompotx-descript,
        createdon TYPE seocompodf-createdon,
        author    TYPE seocompodf-author,
        changedon TYPE seocompodf-changedon,
        changedby TYPE seocompodf-changedby,
      END OF ty_method_detail,

      ty_method_infos TYPE SORTED TABLE OF ty_method_info WITH NON-UNIQUE KEY classname method_name,

      BEGIN OF ty_redef_method_info,
        classname   TYPE seoredef-clsname,
        method_name TYPE seoredef-mtdname,
        is_final    TYPE seoredef-mtdfinal,
      END OF ty_redef_method_info.

    DATA mt_redef_method_info TYPE SORTED TABLE OF ty_redef_method_info WITH UNIQUE KEY classname method_name.
    DATA mf_processing_done TYPE abap_bool.
    DATA mf_only_redefined TYPE abap_bool.
    DATA mt_methods_processed TYPE ty_method_infos.
    DATA mr_results TYPE REF TO zif_sat_ty_object_search=>ty_t_search_result.
    DATA mt_method_details TYPE SORTED TABLE OF ty_method_detail WITH UNIQUE KEY clsname cmpname.
    DATA mo_mtd_key_reader TYPE REF TO zif_sat_method_key_reader.

    DATA:
      BEGIN OF ms_to_process,
        intf_methods     TYPE ty_method_infos,
        standard_methods TYPE ty_method_infos,
      END OF ms_to_process.

    METHODS process_all_methods.
    METHODS process_intf_methods.
    METHODS process_std_methods.

    METHODS determine_method_keys.

    METHODS check_if_implntd_in_curr_cls
      IMPORTING
        it_methods TYPE ty_method_infos.

    METHODS check_if_in_current_clas
      IMPORTING
        it_methods TYPE ty_method_infos.

    METHODS find_next_super_class
      IMPORTING
        VALUE(it_methods) TYPE ty_method_infos.

    METHODS find_next_parent
      IMPORTING
        it_methods TYPE ty_method_infos.

    METHODS read_method_infos.
    METHODS merge_mtd_infos_into_results.
    METHODS read_redef_method_infos.
  ENDCLASS.


CLASS zcl_sat_method_info_reader IMPLEMENTATION.
  METHOD constructor.
    mr_results = ir_results.
    mo_mtd_key_reader = io_method_key_reader.
    mf_only_redefined = if_only_redefined.
  ENDMETHOD.

  METHOD determine_method_keys.
    LOOP AT mr_results->* REFERENCE INTO DATA(lr_result).
      DATA(ls_method_key) = mo_mtd_key_reader->get_method_key( iv_classname   = lr_result->object_name
                                                               iv_method_name = lr_result->method_name ).
      CHECK ls_method_key IS NOT INITIAL.

      " Overwrite the method name with the actual full method name
      lr_result->method_name = ls_method_key-cpdname.

      DATA(ls_method_info) = VALUE ty_method_info( current_class        = ls_method_key-clsname
                                                   original_class_name  = lr_result->object_name
                                                   original_method_name = lr_result->method_name
                                                   result_ref           = lr_result ).

      " is it an interface method??
      IF ls_method_key-cpdname CS '~'.
        SPLIT ls_method_key-cpdname AT '~' INTO ls_method_info-classname ls_method_info-method_name.
        ls_method_info-type = c_method_types-implemented.
      ELSE.
        ls_method_info-method_name = ls_method_key-cpdname.
        ls_method_info-type        = c_method_types-normal.
      ENDIF.

      ls_method_info-status = c_status_flags-look_in_current_cls.

      IF mf_only_redefined = abap_true.
        ls_method_info-is_redefined = abap_true.
        IF ls_method_info-type = c_method_types-implemented.
          mt_methods_processed = VALUE #( BASE mt_methods_processed ( ls_method_info ) ).
        ELSE.
          ls_method_info-status = c_status_flags-find_next_parent.
          ms_to_process-standard_methods = VALUE #( BASE ms_to_process-standard_methods ( ls_method_info ) ).
        ENDIF.
      ELSE.
        IF ls_method_info-type = c_method_types-implemented.
          ms_to_process-intf_methods = VALUE #( BASE ms_to_process-intf_methods ( ls_method_info ) ).
        ELSE.
          ms_to_process-standard_methods = VALUE #( BASE ms_to_process-standard_methods ( ls_method_info ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD apply.
    determine_method_keys( ).

    WHILE mf_processing_done = abap_false.
      process_all_methods( ).
    ENDWHILE.

    " read method information
    read_method_infos( ).
    read_redef_method_infos( ).
    merge_mtd_infos_into_results( ).
  ENDMETHOD.

  METHOD find_next_parent.
  ENDMETHOD.

  METHOD process_all_methods.
    IF ms_to_process-intf_methods IS NOT INITIAL.
      process_intf_methods( ).
    ENDIF.
    IF ms_to_process-standard_methods IS NOT INITIAL.
      process_std_methods( ).
    ENDIF.

    mf_processing_done = xsdbool( ms_to_process-intf_methods IS INITIAL AND ms_to_process-standard_methods IS INITIAL ).
  ENDMETHOD.

  METHOD process_intf_methods.
    DATA(lt_temp_to_process) = ms_to_process-intf_methods.
    CLEAR ms_to_process-intf_methods.

    check_if_implntd_in_curr_cls( lt_temp_to_process ).

    " NOTE: Extra Code if more status flags for interface methods become available
*    LOOP AT lt_temp_to_process REFERENCE INTO DATA(lr_method)
*         GROUP BY lr_method->status
*         INTO DATA(lv_method_status).
*
*      CASE lv_method_status.
*
*        WHEN c_status_flags-look_in_current_cls.
*          check_if_implntd_in_curr_cls( it_methods = VALUE #( FOR tab IN GROUP lv_method_status ( tab ) ) ).
*
*      ENDCASE.
*
*    ENDLOOP.
  ENDMETHOD.

  METHOD process_std_methods.
    DATA(lt_temp_to_process) = ms_to_process-standard_methods.
    CLEAR ms_to_process-standard_methods.

    LOOP AT lt_temp_to_process REFERENCE INTO DATA(lr_method)
         GROUP BY lr_method->status
         INTO DATA(lv_method_status).

      CASE lv_method_status.

        WHEN c_status_flags-look_in_current_cls.
          check_if_in_current_clas( it_methods = VALUE #( FOR tab IN GROUP lv_method_status ( tab ) ) ).

        WHEN c_status_flags-find_next_parent.
          find_next_super_class( it_methods = VALUE #( FOR tab IN GROUP lv_method_status ( tab ) ) ).

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD check_if_implntd_in_curr_cls.
    TYPES: BEGIN OF helper_type,
             clsname    TYPE seometarel-clsname,
             refclsname TYPE seometarel-refclsname,
           END OF helper_type.

    DATA lt_implemented TYPE SORTED TABLE OF helper_type WITH UNIQUE KEY clsname refclsname.

    SELECT DISTINCT implementer~clsname,
           implementer~refclsname
      FROM seometarel AS implementer
      FOR ALL ENTRIES IN @it_methods
      WHERE implementer~clsname = @it_methods-current_class
        AND implementer~refclsname = @it_methods-classname
        AND implementer~reltype = @seor_reltype_implementing
        AND implementer~version = @seoc_version_active
      INTO TABLE @lt_implemented.

    LOOP AT it_methods INTO DATA(ls_method).
      IF NOT line_exists( lt_implemented[ clsname = ls_method-current_class refclsname = ls_method-classname ] ).
        ls_method-is_redefined = abap_true.
      ENDIF.

      mt_methods_processed = VALUE #( BASE mt_methods_processed ( ls_method ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_if_in_current_clas.
    TYPES: BEGIN OF helper_type,
             clsname TYPE seocompo-clsname,
             cmpname TYPE seocompo-cmpname,
             mtdtype TYPE seocompo-mtdtype,
           END OF helper_type.

    DATA lt_found_methods TYPE SORTED TABLE OF helper_type WITH UNIQUE KEY clsname cmpname.

    SELECT DISTINCT method~clsname,
           method~cmpname,
           method~mtdtype
      FROM seocompo AS method
      FOR ALL ENTRIES IN @it_methods
      WHERE method~clsname = @it_methods-current_class
        AND method~cmpname = @it_methods-method_name
        AND method~cmptype = @seoo_cmptype_method
      INTO TABLE @lt_found_methods.

    LOOP AT it_methods INTO DATA(ls_method).
      DATA(lr_found_method) = REF #( lt_found_methods[ clsname = ls_method-current_class cmpname = ls_method-method_name ] OPTIONAL ).
      IF lr_found_method IS BOUND.
        ls_method-result_ref->method_type = lr_found_method->mtdtype.
        ls_method-classname = ls_method-current_class.
        mt_methods_processed = VALUE #( BASE mt_methods_processed ( ls_method ) ).
      ELSE.
        ls_method-status       = c_status_flags-find_next_parent.
        ls_method-is_redefined = abap_true.
        ms_to_process-standard_methods = VALUE #( BASE ms_to_process-standard_methods ( ls_method ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_next_super_class.
    TYPES:
      BEGIN OF ty_super_class,
        clsname    TYPE seometarel-clsname,
        refclsname TYPE seometarel-refclsname,
      END OF ty_super_class.

    DATA lt_super_class TYPE SORTED TABLE OF ty_super_class WITH UNIQUE KEY clsname refclsname.

    SELECT DISTINCT super_class~clsname,
           super_class~refclsname
      FROM seometarel AS super_class
      FOR ALL ENTRIES IN @it_methods
      WHERE super_class~clsname = @it_methods-current_class
        AND super_class~reltype = @seor_reltype_inheritance
        AND super_class~version = @seoc_version_active
      INTO TABLE @lt_super_class.

    IF sy-subrc <> 0.
      " Exception if super class not found???
      RETURN.
    ENDIF.

    LOOP AT it_methods INTO DATA(ls_method).
      TRY.
          DATA(lr_super_class) = REF #( lt_super_class[ clsname = ls_method-current_class ] ).
          ls_method-current_class = lr_super_class->refclsname.
          ls_method-status        = c_status_flags-look_in_current_cls.
          ms_to_process-standard_methods = VALUE #( BASE ms_to_process-standard_methods ( ls_method ) ).
        CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
          " can happen if a final/abstract implemented interface method
          " is found in SEOREDEF (e.g. /BOBF/CL_LIB_B_FRW=>/BOBF/IF_FRW_BUFFER~QUERY)
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_method_infos.
    CHECK mt_methods_processed IS NOT INITIAL.

    SELECT DISTINCT method~clsname,
           method~cmpname,
           method~exposure,
           method~mtddecltyp AS level,
           method~mtdfinal AS is_final,
           method_text~descript,
           method~createdon,
           method~author,
           method~changedon,
           method~changedby
      FROM seocompodf AS method
        LEFT OUTER JOIN seocompotx AS method_text
          ON  method~clsname = method_text~clsname
          AND method~cmpname = method_text~cmpname
          AND method_text~langu = @sy-langu
      FOR ALL ENTRIES IN @mt_methods_processed
      WHERE method~clsname = @mt_methods_processed-classname
        AND method~cmpname = @mt_methods_processed-method_name
        AND method~version = @seoc_version_active
      INTO TABLE @mt_method_details.
  ENDMETHOD.

  METHOD merge_mtd_infos_into_results.
    DATA lt_results TYPE zif_sat_ty_object_search=>ty_t_search_result.

    LOOP AT mt_methods_processed REFERENCE INTO DATA(lr_processed_method).
      DATA(lr_method_detail) = REF #( mt_method_details[ clsname = lr_processed_method->classname
                                                         cmpname = lr_processed_method->method_name ] OPTIONAL ).
      IF lr_method_detail IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(ls_result) = lr_processed_method->result_ref->*.
      ls_result-method_decl_clif   = lr_processed_method->classname.
      ls_result-method_decl_method = lr_processed_method->method_name.
      ls_result-method_descr       = lr_method_detail->descript.
      ls_result-method_exposure    = lr_method_detail->exposure.
      IF mf_only_redefined = abap_false.
        if lr_processed_method->is_redefined = abap_true.
          ls_result-method_is_final = VALUE #( mt_redef_method_info[
                                                   classname   = lr_processed_method->original_class_name
                                                   method_name = lr_processed_method->original_method_name ]-is_final OPTIONAL ).
        else.
        ls_result-method_is_final = lr_method_detail->is_final.
        endif.
      ENDIF.
      ls_result-method_level = lr_method_detail->level.
      ls_result-created_by   = lr_method_detail->author.
      ls_result-created_date = lr_method_detail->createdon.
      ls_result-changed_by   = lr_method_detail->changedby.
      ls_result-changed_date = lr_method_detail->changedon.
      IF lr_processed_method->is_redefined = abap_true.
        ls_result-method_status = zif_sat_c_object_search=>c_method_status_int-redefined.
      ELSEIF lr_processed_method->type = c_method_types-implemented.
        ls_result-method_status = zif_sat_c_object_search=>c_method_status_int-implemented.
      ELSE.
        ls_result-method_status = zif_sat_c_object_search=>c_method_status_int-standard.
      ENDIF.
      lt_results = VALUE #( BASE lt_results ( ls_result ) ).
    ENDLOOP.

    mr_results->* = lt_results.
  ENDMETHOD.

  METHOD read_redef_method_infos.
    CHECK: mf_only_redefined = abap_false,
           line_exists( mt_methods_processed[ is_redefined = abap_true ] ).

    SELECT clsname AS classname,
           mtdname AS method_name,
           mtdfinal AS is_final
      FROM seoredef
      FOR ALL ENTRIES IN @mt_methods_processed
      WHERE clsname = @mt_methods_processed-original_class_name
        AND mtdname = @mt_methods_processed-original_method_name
      INTO CORRESPONDING FIELDS OF TABLE @mt_redef_method_info.
  ENDMETHOD.
ENDCLASS.
