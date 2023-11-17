"! <p class="shorttext synchronized">Filter utilities for CDS Object Search</p>
CLASS zcl_sat_cds_filter_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_recursive_from_usages
      IMPORTING
        iv_entity     TYPE zsat_entity_id
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_t_value_range.
ENDCLASS.


CLASS zcl_sat_cds_filter_util IMPLEMENTATION.
  METHOD get_recursive_from_usages.
    DATA lt_usages TYPE SORTED TABLE OF tabname WITH UNIQUE KEY table_line.
    DATA lt_current_parents TYPE STANDARD TABLE OF tabname.
    DATA lt_tmp_usages LIKE lt_current_parents.

    SELECT DISTINCT base~ddlname
      FROM  zsat_p_cds AS base
        INNER JOIN zsat_i_cdsfrompartentity AS frompart
          ON base~viewname = frompart~ddlviewname
      WHERE frompart~sourceentity = @iv_entity
      INTO TABLE @lt_usages.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_current_parents = lt_usages.

    WHILE lt_current_parents IS NOT INITIAL.
      SELECT DISTINCT base~ddlname
        FROM  zsat_p_cds AS base
          INNER JOIN zsat_i_cdsfrompartentity AS frompart
            ON base~viewname = frompart~ddlviewname
        FOR ALL ENTRIES IN @lt_current_parents
        WHERE frompart~sourceentity = @lt_current_parents-table_line
        INTO TABLE @lt_tmp_usages.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CLEAR lt_current_parents.
      LOOP AT lt_tmp_usages INTO DATA(lv_usage).
        INSERT lv_usage INTO TABLE lt_usages.
        IF sy-subrc = 0.
          lt_current_parents = VALUE #( BASE lt_current_parents
                                        ( lv_usage ) ).
        ENDIF.
      ENDLOOP.

      CLEAR lt_tmp_usages.
    ENDWHILE.

    result = VALUE #( FOR <usage> IN lt_usages
                      ( sign = 'I' option = 'EQ' low = <usage> ) ).
  ENDMETHOD.
ENDCLASS.
