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
      SELECT devclass,
             parentcl AS parent_devclass
        FROM tdevc
        FOR ALL ENTRIES IN @packages_to_read
        WHERE devclass = @packages_to_read-devclass
        INTO CORRESPONDING FIELDS OF TABLE @read_packages.

      CLEAR packages_to_read.

      LOOP AT read_packages ASSIGNING FIELD-SYMBOL(<read_package>).
        CHECK NOT line_exists( packages[ devclass = <read_package>-devclass ] ).

        IF <read_package>-parent_devclass IS NOT INITIAL.
          packages_to_read = VALUE #( BASE packages_to_read ( devclass = <read_package>-parent_devclass ) ).
        ENDIF.

        TRY.
            <read_package>-uri = zcl_sat_adt_util=>map_tadir_obj_to_object_ref(
                                     iv_name = CONV #( <read_package>-devclass )
                                     is_type = VALUE #( objtype_tr = 'DEVC' subtype_wb = 'K' ) )-uri.
          CATCH zcx_adcoset_static_error.
        ENDTRY.

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

      APPEND VALUE #( uri  = <package>-uri
                      type = zif_sat_c_object_types=>package
                      name = <package>-devclass )
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

  METHOD convert.
    mo_devclass_util = NEW lcl_devclass_util( ).
    mo_devclass_util->determine_package_hierarchy( mt_query_result ).
    mo_devclass_util->add_packages_to_adt_result( CHANGING search_result = result ).

    before_conversion( ).

    LOOP AT mt_query_result ASSIGNING FIELD-SYMBOL(<ls_search_result>).
      APPEND INITIAL LINE TO result-objects ASSIGNING FIELD-SYMBOL(<ls_object>).

      convert_result_entry( EXPORTING is_result_entry = <ls_search_result>
                            CHANGING  cs_result       = <ls_object> ).

    ENDLOOP.

    after_conversion( ).
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
                         parent_uri  = mo_devclass_util->get_package_uri( is_result_entry-devclass )
                         description = is_result_entry-description
                         owner       = is_result_entry-created_by
                         created_on  = is_result_entry-created_date
                         changed_by  = is_result_entry-changed_by
                         changed_on  = is_result_entry-changed_date
                         properties  = VALUE #( ( key = 'API_STATE' value = is_result_entry-api_state ) ) ).
  ENDMETHOD.

  METHOD before_conversion.
  ENDMETHOD.

  METHOD after_conversion.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_result_converter_factory IMPLEMENTATION.
  METHOD create_result_converter.
    result = SWITCH #( iv_search_type
                       WHEN zif_sat_c_object_search=>c_search_type-cds_view
                       THEN NEW lcl_cds_result_converter( it_query_result )
                       ELSE NEW lcl_result_converter( it_query_result ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_cds_result_converter IMPLEMENTATION.
  METHOD convert_result_entry.
    super->convert_result_entry( EXPORTING is_result_entry = is_result_entry
                                 CHANGING  cs_result       = cs_result ).

    cs_result-properties = VALUE #( BASE cs_result-properties
                                    ( key = 'SOURCE_TYPE' value = is_result_entry-custom_field_short1 ) ).
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

    SELECT
       FROM ddddlsrc
       FIELDS ddlname,
              source
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
