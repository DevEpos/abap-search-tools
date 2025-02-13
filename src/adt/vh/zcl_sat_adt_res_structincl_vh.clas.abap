"! <p class="shorttext synchronized">Resource for includes of structures</p>
CLASS zcl_sat_adt_res_structincl_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_adt_res_structincl_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_incl_range TYPE RANGE OF tabname.

    IF p_filter_name IS NOT INITIAL.
      lt_incl_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT DISTINCT incl~precfield AS name,
                    text~ddtext    AS description
      FROM dd02l AS struct
             INNER JOIN
               dd03l AS incl ON  struct~tabname = incl~tabname
                             AND incl~fieldname = '.INCLUDE'
                             AND incl~as4local  = 'A'
                 LEFT OUTER JOIN
                   dd02t AS text ON  text~tabname    = incl~precfield
                                 AND text~ddlanguage = @sy-langu
                                 AND text~as4local   = 'A'
      WHERE (    struct~tabclass = 'APPEND'
              OR struct~tabclass = 'INTTAB' )
        AND incl~precfield IN @lt_incl_range
      ORDER BY incl~precfield
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
