"! <p class="shorttext synchronized">Util for Class/Interface filters</p>
CLASS zcl_sat_clif_filter_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Resolves all subclasses from the given list of classes.<br/><br/>
    "!
    "! <strong>Speciality</strong>: Subclasses that are 'leafs' (i.e. do not possess further
    "! subclasses) will be excluded from the result
    CLASS-METHODS resolve_subclasses
      IMPORTING
        it_values     TYPE zif_sat_ty_object_search=>ty_t_value_range
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_value_range.

    "! Reads all implmenting classes for the given interface
    CLASS-METHODS get_intf_implementers
      IMPORTING
        iv_intf_name  TYPE classname
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_value_range.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_class,
        classname  TYPE classname,
        superclass TYPE classname,
      END OF ty_class,
      BEGIN OF ty_super_class,
        classname TYPE classname,
      END OF ty_super_class.
ENDCLASS.


CLASS zcl_sat_clif_filter_util IMPLEMENTATION.
  METHOD resolve_subclasses.
    DATA lt_classes TYPE STANDARD TABLE OF ty_class WITH EMPTY KEY.
    DATA lt_class_hier_temp TYPE SORTED TABLE OF ty_class WITH NON-UNIQUE KEY superclass.
    DATA lt_classes_temp LIKE lt_classes.
    DATA lt_all_super_classes TYPE SORTED TABLE OF ty_super_class WITH UNIQUE KEY classname.

    lt_classes = VALUE #( FOR class IN it_values WHERE ( option = 'EQ' ) ( classname = class-low ) ).

    IF lt_classes IS INITIAL.
      RETURN.
    ENDIF.

    WHILE lt_classes IS NOT INITIAL.
      SELECT super_class~clsname AS classname,
             super_class~refclsname AS superclass
        FROM seometarel AS super_class
        FOR ALL ENTRIES IN @lt_classes
        WHERE super_class~refclsname = @lt_classes-classname
          AND super_class~reltype = @seor_reltype_inheritance
          AND super_class~version = @seoc_version_active
        INTO TABLE @lt_classes_temp.

      INSERT LINES OF lt_classes_temp INTO TABLE lt_class_hier_temp.
      lt_classes = lt_classes_temp.
    ENDWHILE.

    LOOP AT lt_class_hier_temp REFERENCE INTO DATA(lr_class_hier_entry).
      INSERT VALUE #( classname = lr_class_hier_entry->superclass ) INTO TABLE lt_all_super_classes.
    ENDLOOP.

    " delete all classes that do not have parent class
    LOOP AT lt_class_hier_temp REFERENCE INTO DATA(lr_class).
      IF line_exists( lt_all_super_classes[ classname = lr_class->classname ] ).
        result = VALUE #( BASE result ( sign = 'I' option = 'EQ' low = lr_class->classname ) ).
      ENDIF.
      DELETE lt_class_hier_temp.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_intf_implementers.
    SELECT 'I' AS sign,
           'EQ' AS option,
           clsname AS low
      FROM seometarel
      WHERE reltype = @seor_reltype_implementing
        AND refclsname = @iv_intf_name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

ENDCLASS.
