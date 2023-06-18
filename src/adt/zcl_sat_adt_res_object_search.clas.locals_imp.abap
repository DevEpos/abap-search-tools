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
                      type = 'DEVC/K'
                      name = <package>-devclass )
             TO search_result-objects ASSIGNING FIELD-SYMBOL(<package_adt_result>).

      IF <package>-parent_devclass IS NOT INITIAL.
        <package_adt_result>-parent_uri = get_package_uri( <package>-parent_devclass ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
