*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_devclass_util IMPLEMENTATION.
  METHOD determine_package_hierarchy.
    DATA packages_to_read TYPE STANDARD TABLE OF ty_package.
    DATA read_packages    TYPE STANDARD TABLE OF ty_package.

    packages_to_read = CORRESPONDING #( search_results ).
    SORT packages_to_read.
    DELETE ADJACENT DUPLICATES FROM packages_to_read.

    WHILE packages_to_read IS NOT INITIAL.
      SELECT p~devclass,
             parentcl AS parent_devclass,
             t~ctext
        FROM tdevc AS p
          LEFT OUTER JOIN tdevct AS t
            ON p~devclass = t~devclass
            AND t~spras = @sy-langu
        FOR ALL ENTRIES IN @packages_to_read
        WHERE p~devclass = @packages_to_read-devclass
        INTO CORRESPONDING FIELDS OF TABLE @read_packages.

      CLEAR packages_to_read.

      LOOP AT read_packages ASSIGNING FIELD-SYMBOL(<read_package>).
        CHECK NOT line_exists( packages[ devclass = <read_package>-devclass ] ).

        IF <read_package>-parent_devclass IS NOT INITIAL.
          packages_to_read = VALUE #( BASE packages_to_read ( devclass = <read_package>-parent_devclass ) ).
        ENDIF.

        <read_package>-uri = zcl_sat_adt_util=>map_tadir_obj_to_object_ref(
                                 iv_name = CONV #( <read_package>-devclass )
                                 is_type = VALUE #( objtype_tr = 'DEVC' subtype_wb = 'K' ) )-uri.

        packages = VALUE #( BASE packages ( <read_package> ) ).
      ENDLOOP.

    ENDWHILE.
  ENDMETHOD.

  METHOD get_package_uri.
    CHECK packages IS NOT INITIAL.

    result = VALUE #( packages[ devclass = package_name ]-uri OPTIONAL ).
  ENDMETHOD.

  METHOD add_packages_to_adt_result.
    LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).

      APPEND VALUE #( uri         = <package>-uri
                      type        = zif_sat_c_object_types=>package
                      name        = <package>-devclass
                      description = <package>-ctext )
             TO search_result-objects ASSIGNING FIELD-SYMBOL(<package_adt_result>).

      IF <package>-parent_devclass IS NOT INITIAL.
        <package_adt_result>-parent_uri = get_package_uri( <package>-parent_devclass ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_result_converter IMPLEMENTATION.
  METHOD constructor.
    mt_query_result = it_query_result.
  ENDMETHOD.

  METHOD convert_entries.
    LOOP AT mt_query_result ASSIGNING FIELD-SYMBOL(<ls_search_result>).
      APPEND INITIAL LINE TO cs_result-objects ASSIGNING FIELD-SYMBOL(<ls_object>).

      convert_result_entry( EXPORTING is_result_entry = <ls_search_result>
                            CHANGING  cs_result       = <ls_object> ).

      cs_result-count = cs_result-count + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_sat_adt_objs_res_converter~convert.
    IF mf_no_package_hierarchy = abap_false.
      mo_devclass_util = NEW lcl_devclass_util( ).
      mo_devclass_util->determine_package_hierarchy( mt_query_result ).
      mo_devclass_util->add_packages_to_adt_result( CHANGING search_result = result ).
    ENDIF.

    before_conversion( ).
    convert_entries( CHANGING cs_result = result ).
    after_conversion( CHANGING cs_result = result ).
  ENDMETHOD.

  METHOD convert_result_entry.
    DATA(ls_object_reference) = zcl_sat_adt_util=>create_adt_uri( iv_type       = is_result_entry-entity_type
                                                                  iv_tadir_type = is_result_entry-tadir_type
                                                                  iv_name       = is_result_entry-object_name
                                                                  iv_name2      = is_result_entry-alt_object_name ).

    cs_result = VALUE #( name        = is_result_entry-object_name
                         alt_name    = is_result_entry-raw_object_name
                         devclass    = is_result_entry-devclass
                         type        = ls_object_reference-type
                         uri         = ls_object_reference-uri
                         parent_uri  = COND #( WHEN mo_devclass_util IS BOUND
                                               THEN mo_devclass_util->get_package_uri( is_result_entry-devclass ) )
                         description = is_result_entry-description
                         owner       = is_result_entry-created_by
                         created_on  = is_result_entry-created_date
                         changed_by  = is_result_entry-changed_by
                         changed_on  = is_result_entry-changed_date ).
  ENDMETHOD.

  METHOD before_conversion.
  ENDMETHOD.

  METHOD after_conversion.
  ENDMETHOD.

  METHOD set_no_package_hiearchy.
    mf_no_package_hierarchy = if_value.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_result_converter_factory IMPLEMENTATION.
  METHOD create_result_converter.
    result = SWITCH #( iv_search_type
                       WHEN zif_sat_c_object_search=>c_search_type-cds_view THEN
                         NEW lcl_cds_result_converter( it_query_result )
                       WHEN zif_sat_c_object_search=>c_search_type-method THEN
                         NEW lcl_method_result_converter( it_query_result )
                       WHEN zif_sat_c_object_search=>c_search_type-message THEN
                         NEW lcl_message_result_converter( it_query_result )
                       ELSE
                         NEW lcl_result_converter( it_query_result ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_cds_result_converter IMPLEMENTATION.
  METHOD convert_result_entry.
    super->convert_result_entry( EXPORTING is_result_entry = is_result_entry
                                 CHANGING  cs_result       = cs_result ).

    cs_result-properties = VALUE #( BASE cs_result-properties
                                    ( key = 'SOURCE_TYPE' value = is_result_entry-cds_source_type ) ).
    set_ddl_positional_uri( EXPORTING is_result_entity = is_result_entry
                            CHANGING  cs_result        = cs_result ).
  ENDMETHOD.

  METHOD before_conversion.
    " Positional URI is no longer needed for where used starting with NW 7.54
    CHECK sy-saprl < 754.

    " Read sources of all found DDLS search results to get row/column where the name of
    " the entity is starting
    read_ddl_sources( ).
  ENDMETHOD.

  METHOD read_ddl_sources.
    DATA lt_ddlname TYPE ty_lt_ddlname.

    lt_ddlname = VALUE #( FOR res IN mt_query_result
                          WHERE
                                ( tadir_type = 'DDLS' )
                          ( sign = 'I' option = 'EQ' low = res-alt_object_name ) ).

    SELECT ddlname,
           source
       FROM ddddlsrc
       WHERE as4local = 'A'
         AND ddlname  IN @lt_ddlname
    INTO CORRESPONDING FIELDS OF TABLE @mt_ddls_source.
  ENDMETHOD.

  METHOD set_ddl_positional_uri.
    ASSIGN mt_ddls_source[ ddlname = is_result_entity-alt_object_name ] TO FIELD-SYMBOL(<ls_source>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    zcl_sat_cds_view_factory=>get_entityname_pos_in_ddlsrc( EXPORTING iv_entity_id = is_result_entity-object_name
                                                                      iv_source    = <ls_source>-source
                                                            IMPORTING ev_column    = DATA(lv_col)
                                                                      ev_row       = DATA(lv_row) ).
    IF lv_col <> -1 AND lv_row <> -1.
      " Adjust ADT URI
      cs_result-uri = |{ cs_result-uri }{ zif_sat_c_adt_utils=>c_ddl_pos_uri_segment }{ lv_row },{ lv_col }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_method_result_converter IMPLEMENTATION.
  METHOD convert_entries.
    DATA lt_method_entries TYPE zif_sat_ty_adt_types=>ty_t_adt_obj_ref.

    LOOP AT mt_query_result ASSIGNING FIELD-SYMBOL(<ls_query_result>)
         GROUP BY <ls_query_result>-object_name.

      " 1) create ADT URI for Class/Interface
      DATA(ls_object_reference) = zcl_sat_adt_util=>create_adt_uri( iv_type       = <ls_query_result>-entity_type
                                                                    iv_tadir_type = <ls_query_result>-tadir_type
                                                                    iv_name       = <ls_query_result>-object_name
                                                                    iv_name2      = <ls_query_result>-alt_object_name ).
      DATA(ls_result_entry) = VALUE zif_sat_ty_adt_types=>ty_s_adt_obj_ref(
                                        name        = <ls_query_result>-object_name
                                        alt_name    = <ls_query_result>-raw_object_name
                                        devclass    = <ls_query_result>-devclass
                                        type        = ls_object_reference-type
                                        uri         = ls_object_reference-uri
                                        parent_uri  = mo_devclass_util->get_package_uri( <ls_query_result>-devclass )
                                        description = <ls_query_result>-description
                                        owner       = <ls_query_result>-created_by
                                        created_on  = <ls_query_result>-created_date
                                        changed_by  = <ls_query_result>-changed_by
                                        changed_on  = <ls_query_result>-changed_date ).

      CLEAR lt_method_entries.

      " 2) create method entries
      LOOP AT GROUP <ls_query_result> ASSIGNING FIELD-SYMBOL(<ls_method>).
        DATA(ls_obj_type) = VALUE wbobjtype(
                                      objtype_tr = <ls_method>-tadir_type
                                      subtype_wb = COND #( WHEN <ls_method>-tadir_type = zif_sat_c_tadir_types=>class
                                                           THEN c_sub_obj_type-class_method_impl
                                                           ELSE c_sub_obj_type-intf_method ) ).

        IF <ls_method>-tadir_type = zif_sat_c_tadir_types=>class.
          IF <ls_method>-method_is_abstract = abap_true. " abstract
            ls_obj_type-subtype_wb = c_sub_obj_type-class_method_def.
          ENDIF.
        ENDIF.

        DATA(lv_uri) = map_method_to_uri( iv_clif_name   = <ls_query_result>-object_name
                                          iv_type        = ls_obj_type
                                          iv_method_name = <ls_method>-method_name ).

        IF lv_uri IS NOT INITIAL.
          " reset the sub type to OM if necessary, so ADT produces the correct icon
          IF ls_obj_type-subtype_wb = c_sub_obj_type-class_method_def.
            ls_obj_type-subtype_wb = c_sub_obj_type-class_method_impl.
          ENDIF.
          lt_method_entries = VALUE #( BASE lt_method_entries
                                       ( name        = <ls_method>-method_name
                                         type        = ls_obj_type-objtype_tr && '/' && ls_obj_type-subtype_wb
                                         description = <ls_method>-method_descr
                                         parent_uri  = ls_object_reference-uri
                                         uri         = lv_uri
                                         properties  = fill_method_properties( <ls_method> ) ) ).
          cs_result-count   = cs_result-count + 1.
        ENDIF.
      ENDLOOP.

      IF lt_method_entries IS NOT INITIAL.
        cs_result-objects = VALUE #( BASE cs_result-objects ( ls_result_entry ) ( LINES OF lt_method_entries ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_method_to_uri.
    DATA lo_wb_request  TYPE REF TO cl_wb_request.
    DATA lv_object_name TYPE seu_objkey.

    IF iv_type-objtype_tr = zif_sat_c_tadir_types=>class AND iv_type-subtype_wb = swbm_c_type_cls_mtd_impl.
      result = |/sap/bc/adt/oo/classes/{ to_lower( cl_http_utility=>escape_url( CONV #( iv_clif_name ) ) ) }| &&
               |/source/main#type={ iv_type-objtype_tr }/{ iv_type-subtype_wb };name={ to_lower( iv_method_name ) }|.
      RETURN.
    ENDIF.

    lv_object_name(30) = iv_clif_name.
    lv_object_name+30 = iv_method_name.
    CREATE OBJECT lo_wb_request
      EXPORTING  p_global_type                = iv_type
                 p_object_name                = lv_object_name
                 p_operation                  = swbm_c_op_disp_or_edit
      EXCEPTIONS illegal_object_type          = 1
                 illegal_operation            = 2
                 illegal_new_window_parameter = 3
                 OTHERS                       = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lo_src_map_opts) = cl_adt_tools_core_factory=>get_instance( )->create_source_mapping_options( ).
    lo_src_map_opts->set_use_source_based_position( use_source_based_position = abap_true ).
    lo_src_map_opts->set_use_source_main( abap_true ).

    TRY.
        result = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( )->map_wb_request_to_objref(
                     wb_request      = lo_wb_request
                     mapping_options = lo_src_map_opts )->ref_data-uri.
      CATCH cx_adt_uri_mapping.
    ENDTRY.
  ENDMETHOD.

  METHOD fill_method_properties.
    IF is_method-method_is_abstract = abap_true.
      result = VALUE #( ( key = 'isAbstract' value = abap_true type = zif_sat_c_adt_utils=>c_property_type-bool ) ).
    ENDIF.

    IF is_method-method_is_final = abap_true.
      result = VALUE #( BASE result ( key = 'isFinal' value = abap_true type = zif_sat_c_adt_utils=>c_property_type-bool ) ).
    ENDIF.

    IF is_method-method_level = seoo_mtddecltyp_class_method.
      result = VALUE #( BASE result ( key = 'isStatic' value = abap_true type = zif_sat_c_adt_utils=>c_property_type-bool ) ).
    ENDIF.

    IF is_method-method_status = zif_sat_c_object_search=>c_method_status_int-redefined.
      result = VALUE #( BASE result ( key = 'isRedefined' value = abap_true type = zif_sat_c_adt_utils=>c_property_type-bool ) ).
    ENDIF.

    IF is_method-method_type = seoo_mtdtype_constructor.
      result = VALUE #( BASE result ( key = 'isConstructor' value = abap_true type = zif_sat_c_adt_utils=>c_property_type-bool ) ).
    ELSEIF is_method-method_type = seoo_mtdtype_eventhandler.
      result = VALUE #( BASE result ( key = 'isEventHandler' value = abap_true type = zif_sat_c_adt_utils=>c_property_type-bool ) ).
    ENDIF.

    result = VALUE #(
        BASE result
        ( key   = 'visibility'
          value = SWITCH string( is_method-method_exposure
                                 WHEN seoc_exposure_public    THEN zif_sat_c_object_search=>c_visibility-public
                                 WHEN seoc_exposure_protected THEN zif_sat_c_object_search=>c_visibility-protected
                                 WHEN seoc_exposure_private   THEN zif_sat_c_object_search=>c_visibility-private ) ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_message_result_converter IMPLEMENTATION.
  METHOD convert_entries.
    LOOP AT mt_query_result ASSIGNING FIELD-SYMBOL(<ls_query_result>)
         GROUP BY <ls_query_result>-object_name.

      " 1) create ADT URI for Message Class
      DATA(ls_object_reference) = zcl_sat_adt_util=>create_adt_uri( iv_tadir_type = <ls_query_result>-tadir_type
                                                                    iv_name       = <ls_query_result>-object_name ).
      DATA(ls_result_entry) = VALUE zif_sat_ty_adt_types=>ty_s_adt_obj_ref(
                                        name        = <ls_query_result>-object_name
                                        alt_name    = <ls_query_result>-raw_object_name
                                        devclass    = <ls_query_result>-devclass
                                        type        = ls_object_reference-type
                                        uri         = ls_object_reference-uri
                                        parent_uri  = mo_devclass_util->get_package_uri( <ls_query_result>-devclass )
                                        description = <ls_query_result>-description ).

      cs_result-objects = VALUE #( BASE cs_result-objects ( ls_result_entry ) ).

      " 2) create message entries
      LOOP AT GROUP <ls_query_result> ASSIGNING FIELD-SYMBOL(<ls_message>).
        DATA(lv_uri) = `/sap/bc/adt/messageclass/` &&
                       to_lower( cl_http_utility=>escape_url( unescaped = |{ <ls_message>-object_name }| ) ) &&
                       |/messages/{ <ls_message>-message_number }|.

        cs_result-objects = VALUE #(
            BASE cs_result-objects
            ( name       = |[{ <ls_message>-message_number }] { <ls_message>-message_short_text }|
              type       = zif_sat_c_object_types=>message
              parent_uri = ls_object_reference-uri
              changed_by = <ls_message>-changed_by
              changed_on = <ls_message>-changed_date
              uri        = lv_uri ) ).
        cs_result-count   = cs_result-count + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
