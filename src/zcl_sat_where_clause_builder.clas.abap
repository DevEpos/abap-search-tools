CLASS zcl_sat_where_clause_builder DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Add selopt table as new OR seltab to AND tab</p>
    CLASS-METHODS add_selopt_as_or_seltab
      IMPORTING
        it_select_option TYPE ANY TABLE
        iv_sqlfieldname  TYPE zif_sat_ty_global=>ty_sql_fieldname
        iv_sql_function  TYPE zsat_sql_function OPTIONAL
      CHANGING
        ct_and_seltab    TYPE zif_sat_ty_global=>ty_t_and_seltab_sql.

    "! <p class="shorttext synchronized">Add selopt values as new Line in OR seltab list</p>
    CLASS-METHODS add_selopt_to_or_seltab
      IMPORTING
        it_select_option TYPE ANY TABLE
        iv_sqlfieldname  TYPE zif_sat_ty_global=>ty_sql_fieldname
        iv_sql_function  TYPE zsat_sql_function OPTIONAL
      CHANGING
        ct_or_seltab     TYPE zif_sat_ty_global=>ty_t_or_seltab_sql.

    "! <p class="shorttext synchronized">Creates OR seltab from the given selopt</p>
    "!
    "! @parameter it_select_option | <p class="shorttext synchronized">Selopt to be tranferred</p>
    "! @parameter iv_sqlfieldname  | <p class="shorttext synchronized">The fieldname to be used</p>
    "! @parameter rs_or_seltab     | <p class="shorttext synchronized">The created OR seltab</p>
    CLASS-METHODS create_or_seltab
      IMPORTING
        it_select_option    TYPE ANY TABLE
        iv_sqlfieldname     TYPE zif_sat_ty_global=>ty_sql_fieldname
        iv_sql_function     TYPE zsat_sql_function OPTIONAL
      RETURNING
        VALUE(rs_or_seltab) TYPE zif_sat_ty_global=>ty_s_or_seltab_sql.

    "! <p class="shorttext synchronized">Creates where condition table connected with AND keyword</p>
    "!
    "! @parameter it_and_seltab | List of or conditions to be connected via AND
    "! @parameter rt_where      | Lines of conditions for SQL where clause
    CLASS-METHODS create_and_condition
      IMPORTING
        it_and_seltab   TYPE zif_sat_ty_global=>ty_t_and_seltab_sql
      RETURNING
        VALUE(rt_where) TYPE string_table.

    CLASS-METHODS get_option
      IMPORTING
        iv_sign        TYPE ddsign
        iv_option      TYPE ddoption
        iv_high        TYPE clike
        if_escape_char TYPE abap_bool OPTIONAL
      CHANGING
        cv_option      TYPE ddoption
        cv_low         TYPE clike.

    "! <p class="shorttext synchronized">Creates where condition table connected with OR keyword</p>
    "!
    "! @parameter it_or_seltab | List of conditions to be connected via OR
    "! @parameter rt_where     | Lines of conditions for SQL where clause
    CLASS-METHODS create_or_condition
      IMPORTING
        it_or_seltab    TYPE zif_sat_ty_global=>ty_t_or_seltab_sql
      RETURNING
        VALUE(rt_where) TYPE string_table.

  PRIVATE SECTION.
    TYPES ty_filter_value TYPE c LENGTH 100.
    TYPES: BEGIN OF ty_s_selopt.
             INCLUDE TYPE zif_sat_ty_global=>ty_s_selopt.
    TYPES:   subquery TYPE string.
    TYPES: END OF ty_s_selopt.
    TYPES ty_t_selopt TYPE STANDARD TABLE OF ty_s_selopt WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_field_selection,
        sqlfieldname TYPE zif_sat_ty_global=>ty_sql_fieldname,
        field        TYPE zsat_fieldname_with_alias,
        sql_function TYPE zsat_sql_function,
        options      TYPE ty_t_selopt,
        convert      TYPE rsconvert,
      END OF ty_s_field_selection.
    TYPES ty_where_line TYPE c LENGTH 120.
    TYPES ty_t_where_clause TYPE STANDARD TABLE OF ty_where_line.
    TYPES ty_t_field_selection TYPE STANDARD TABLE OF ty_s_field_selection WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_s_where_info,
        fieldname TYPE fieldname,
        ilength   TYPE i,
      END   OF ty_s_where_info.

    CONSTANTS c_line_length TYPE i VALUE 120.

    CLASS-METHODS free_selections_to_where
      IMPORTING
        it_field_range  TYPE ty_t_field_selection
      RETURNING
        VALUE(rt_where) TYPE string_table.

    CLASS-METHODS create_condition
      IMPORTING
        it_sel          TYPE zif_sat_ty_global=>ty_t_seltab_sql
      RETURNING
        VALUE(rt_where) TYPE string_table.

    CLASS-METHODS where_for_one_field
      IMPORTING
        is_field_sel TYPE ty_s_field_selection
      CHANGING
        ct_where     TYPE ty_t_where_clause
        cv_subrc     LIKE sy-subrc.

    CLASS-METHODS add_fieldname_to_clause
      IMPORTING
        iv_fieldname    TYPE zif_sat_ty_global=>ty_sql_fieldname
        iv_sql_function TYPE zsat_sql_function OPTIONAL
      CHANGING
        ct_where        TYPE ty_t_where_clause
        cv_where        TYPE ty_where_line
        cv_offset       TYPE i
        cv_low          TYPE ty_filter_value   OPTIONAL
        cv_high         TYPE ty_filter_value   OPTIONAL.

    CLASS-METHODS where_single_word_new
      IMPORTING
        iv_word        TYPE string
        iv_word_length TYPE i
      CHANGING
        ct_where       TYPE ty_t_where_clause
        cv_where       TYPE ty_where_line
        cv_offset      TYPE i.

    CLASS-METHODS single_clause_new
      IMPORTING
        iv_fieldname        TYPE zif_sat_ty_global=>ty_sql_fieldname
        iv_sql_function     TYPE zsat_sql_function OPTIONAL
        is_option           TYPE ty_s_selopt
        iv_fieldname_length TYPE i
      CHANGING
        ct_where            TYPE ty_t_where_clause
        cv_where            TYPE ty_where_line
        cv_offset           TYPE i.

    CLASS-METHODS single_subquery_clause_new
      IMPORTING
        iv_fieldname        TYPE zif_sat_ty_global=>ty_sql_fieldname
        iv_subquery         TYPE string
        iv_option           TYPE ddoption
        iv_fieldname_length TYPE i
      CHANGING
        ct_where            TYPE ty_t_where_clause
        cv_where            TYPE ty_where_line
        cv_offset           TYPE i.

    CLASS-METHODS start_new_line
      CHANGING
        ct_where  TYPE ty_t_where_clause
        cv_where  TYPE ty_where_line
        cv_offset TYPE i.
ENDCLASS.


CLASS zcl_sat_where_clause_builder IMPLEMENTATION.
  METHOD add_selopt_as_or_seltab.
    DATA(lt_or_seltab) = VALUE zif_sat_ty_global=>ty_t_or_seltab_sql( ).
    add_selopt_to_or_seltab( EXPORTING it_select_option = it_select_option
                                       iv_sqlfieldname  = iv_sqlfieldname
                                       iv_sql_function  = iv_sql_function
                             CHANGING  ct_or_seltab     = lt_or_seltab ).
    IF lt_or_seltab IS INITIAL.
      RETURN.
    ENDIF.

    ct_and_seltab = VALUE #( BASE ct_and_seltab ( lt_or_seltab ) ).
  ENDMETHOD.

  METHOD add_selopt_to_or_seltab.
    DATA(ls_or_seltab) = create_or_seltab( it_select_option = it_select_option
                                           iv_sqlfieldname  = iv_sqlfieldname
                                           iv_sql_function  = iv_sql_function ).
    IF ls_or_seltab-values IS INITIAL.
      RETURN.
    ENDIF.
    ct_or_seltab = VALUE #( BASE ct_or_seltab ( ls_or_seltab ) ).
  ENDMETHOD.

  METHOD create_or_seltab.
    CHECK it_select_option IS NOT INITIAL.

    LOOP AT it_select_option ASSIGNING FIELD-SYMBOL(<ls_selopt>).
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_selopt> TO FIELD-SYMBOL(<lv_sign>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_selopt> TO FIELD-SYMBOL(<lv_option>).
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_selopt> TO FIELD-SYMBOL(<lv_low>).
      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_selopt> TO FIELD-SYMBOL(<lv_high>).

      CHECK     <lv_sign>   IS ASSIGNED
            AND <lv_option> IS ASSIGNED
            AND <lv_low>    IS ASSIGNED
            AND <lv_high>   IS ASSIGNED.

      rs_or_seltab-values = VALUE #( BASE rs_or_seltab-values
                                     ( sqlfieldname = iv_sqlfieldname
                                       sql_function = iv_sql_function
                                       sign         = <lv_sign>
                                       option       = <lv_option>
                                       low          = |{ <lv_low> ALIGN = LEFT }|
                                       high         = |{ <lv_high> ALIGN = LEFT }| ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_and_condition.
    DATA(lf_multi_and) = xsdbool( lines( it_and_seltab ) > 1 ).
    " Table it_or_seltab contains a counter and a deep table that contains
    " the selection criteria for this counter.

    IF lf_multi_and = abap_true.
      rt_where = VALUE #( ( `(` ) ).
    ENDIF.

    LOOP AT it_and_seltab ASSIGNING FIELD-SYMBOL(<lt_or_seltab>).
      IF sy-tabix > 1.
        rt_where = VALUE #( BASE rt_where (  `) AND (` ) ).
      ENDIF.

      DATA(lf_multi_or) = xsdbool( lines( <lt_or_seltab> ) > 1 ).
      IF lf_multi_or = abap_true.
        rt_where = VALUE #( BASE rt_where ( `(` ) ).
      ENDIF.

      LOOP AT <lt_or_seltab> ASSIGNING FIELD-SYMBOL(<ls_or_seltab>).
        IF sy-tabix > 1.
          rt_where = VALUE #( BASE rt_where (  `) OR (` ) ).
        ENDIF.

        rt_where = VALUE #( BASE rt_where
                            ( LINES OF create_condition( it_sel = <ls_or_seltab>-values ) ) ).
      ENDLOOP.

      IF lf_multi_or = abap_true.
        rt_where = VALUE #( BASE rt_where ( `)` ) ).
      ENDIF.

    ENDLOOP.

    IF lf_multi_and = abap_true.
      rt_where = VALUE #( BASE rt_where ( `)` ) ).
    ENDIF.
  ENDMETHOD.

  METHOD create_condition.
    DATA lt_field_range TYPE ty_t_field_selection.
    DATA ls_field_range TYPE ty_s_field_selection.
    DATA ls_selopt TYPE ty_s_selopt.
    DATA lv_field_length TYPE i.
    DATA lv_old_field TYPE zif_sat_ty_global=>ty_sql_fieldname.

    CHECK it_sel IS NOT INITIAL.

    LOOP AT it_sel ASSIGNING FIELD-SYMBOL(<ls_selfield>).
      " Handle incomplete options
      DATA(lv_fieldname) = COND #( WHEN <ls_selfield>-sqlfieldname IS NOT INITIAL
                                   THEN <ls_selfield>-sqlfieldname
                                   ELSE <ls_selfield>-field ).
      CHECK lv_fieldname IS NOT INITIAL.

      " New fieldname
      IF     lv_fieldname <> lv_old_field
         AND lv_old_field <> space.
        ls_field_range-convert-where_leng = ls_field_range-convert-clength.
        ls_field_range-convert-olength    = ls_field_range-convert-clength.
        ls_field_range-convert-length     = ls_field_range-convert-clength * cl_abap_char_utilities=>charsize.
        lt_field_range = VALUE #( BASE lt_field_range ( ls_field_range ) ).
        CLEAR: ls_field_range,
               lv_field_length.
      ENDIF.

      ls_field_range-field        = <ls_selfield>-field.
      ls_field_range-sqlfieldname = <ls_selfield>-sqlfieldname.
      ls_field_range-sql_function = <ls_selfield>-sql_function.

      IF ls_field_range-sqlfieldname IS INITIAL.
        ls_field_range-sqlfieldname = ls_field_range-field.
      ENDIF.

      ls_field_range-convert-type = 'C'.

      ls_selopt = VALUE #( sign     = COND #( WHEN <ls_selfield>-sign = space THEN 'I' ELSE <ls_selfield>-sign )
                           low      = <ls_selfield>-low
                           high     = <ls_selfield>-high
                           option   = <ls_selfield>-option
                           subquery = <ls_selfield>-subquery ).

      get_option( EXPORTING iv_sign   = ls_selopt-sign
                            iv_option = <ls_selfield>-option
                            iv_high   = <ls_selfield>-high
                  CHANGING  cv_option = ls_selopt-option
                            cv_low    = ls_selopt-low ).

      lv_field_length = strlen( <ls_selfield>-low ).
      IF lv_field_length = 0.
        lv_field_length = 1.
      ENDIF.

      IF lv_field_length > ls_field_range-convert-clength.
        ls_field_range-convert-clength = lv_field_length.
      ENDIF.

      lv_field_length = strlen( <ls_selfield>-high ).
      IF lv_field_length > ls_field_range-convert-clength.
        ls_field_range-convert-clength = lv_field_length.
      ENDIF.

      ls_field_range-options = VALUE #( BASE ls_field_range-options ( ls_selopt ) ).
      lv_old_field = lv_fieldname.
    ENDLOOP.

    " Complete last field values
    IF ls_field_range-sqlfieldname IS NOT INITIAL.
      ls_field_range-convert-where_leng = ls_field_range-convert-clength.
      ls_field_range-convert-olength    = ls_field_range-convert-clength.
      ls_field_range-convert-length     = ls_field_range-convert-clength * cl_abap_char_utilities=>charsize.
      lt_field_range = VALUE #( BASE lt_field_range ( ls_field_range ) ).
    ENDIF.

    IF lt_field_range IS INITIAL.
      RETURN.
    ENDIF.

    rt_where = free_selections_to_where( it_field_range = lt_field_range ).
  ENDMETHOD.

  METHOD create_or_condition.
    DATA(lt_or_seltab) = it_or_seltab.

    " first check if every line really contains selections
    DELETE lt_or_seltab WHERE values IS INITIAL.
    IF lt_or_seltab IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lf_multi) = xsdbool( lines( lt_or_seltab ) > 1 ).
    " Table it_or_seltab contains a counter and a deep table that contains
    " the selection criteria for this counter.

    IF lf_multi = abap_true.
      rt_where = VALUE #( ( `(` ) ).
    ENDIF.

    LOOP AT lt_or_seltab ASSIGNING FIELD-SYMBOL(<ls_or_seltab>).
      IF sy-tabix > 1.
        rt_where = VALUE #( BASE rt_where (  `) or (` ) ).
      ENDIF.
      rt_where = VALUE #( BASE rt_where
                          ( LINES OF create_condition( it_sel = <ls_or_seltab>-values ) ) ).

    ENDLOOP.
    IF lf_multi = abap_true.
      rt_where = VALUE #( BASE rt_where ( `)` ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_option.
    DATA lf_low_cp TYPE abap_bool.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_sign) = COND #( WHEN iv_sign = space THEN 'I' ELSE iv_sign ).
    DATA(lf_escape_char) = if_escape_char.

    IF     ( cv_low CS '*' OR cv_low CS '+' )
       AND ( lf_escape_char <> abap_true ).
      lf_low_cp = abap_true.
    ENDIF.

    " ONLY RELEVANT FOR POOL TABLES
    " ---------------------------------------------------------------------
    " IF cv_low     CS '_' AND
    " lf_low_cp = abap_true.
    " sy-subrc = 0.
    " WHILE sy-subrc = 0.
    " REPLACE '_' WITH '+' INTO cv_low.
    " ENDWHILE.
    " ENDIF.
    " --
    " IF cv_low     CS '%' AND
    " lf_low_cp = abap_true.
    " sy-subrc = 0.
    " WHILE sy-subrc = 0.
    " REPLACE '%' WITH '+' INTO cv_low.
    " ENDWHILE.
    " ENDIF.
    " ---------------------------------------------------------------------

    IF iv_option = space.
      IF     iv_high <> space
         AND cv_low  <> iv_high.
        cv_option = zif_sat_c_options=>between.
      ENDIF.
      IF lf_low_cp = abap_true AND iv_high IS INITIAL.
        cv_option = zif_sat_c_options=>contains_pattern.
      ENDIF.
      IF cv_option = space.
        cv_option = zif_sat_c_options=>equals.
      ENDIF.
    ELSE.
      cv_option = iv_option.
    ENDIF.

    IF cv_option = space.
      cv_option = zif_sat_c_options=>equals.
    ENDIF.
  ENDMETHOD.

  METHOD free_selections_to_where.
    DATA l_subrc TYPE sy-subrc.
    DATA lt_where TYPE ty_t_where_clause.

    LOOP AT it_field_range ASSIGNING FIELD-SYMBOL(<ls_field_selection>).
      CLEAR lt_where.

      where_for_one_field( EXPORTING is_field_sel = <ls_field_selection>
                           CHANGING  ct_where     = lt_where
                                     cv_subrc     = l_subrc ).
      CHECK l_subrc = 0.

      IF rt_where IS INITIAL.
        rt_where = lt_where.
      ELSE.
        ASSIGN lt_where[ 1 ] TO FIELD-SYMBOL(<lv_where>).
        IF sy-subrc = 0.
          <lv_where>(3) = 'AND'.
          rt_where = VALUE #( BASE rt_where ( LINES OF CORRESPONDING #( lt_where ) ) ).
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD where_for_one_field.
    DATA lv_fieldname_length TYPE i.
    DATA lv_and_offset TYPE i VALUE 4. " Space for 'AND' word
    DATA l_i_num TYPE i.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_info TYPE ty_s_where_info.
    DATA lv_where TYPE ty_where_line.

    FIELD-SYMBOLS <ls_option> TYPE ty_s_selopt.

    " always use length of name to spare some spaces
    lv_fieldname_length = strlen( is_field_sel-sqlfieldname ).
    IF NOT lv_fieldname_length > 0.
      lv_fieldname_length = 1.
    ENDIF.

    TRY.
        cl_abap_dyn_prg=>check_column_name( is_field_sel-field ).
      CATCH cx_abap_invalid_name.
        MESSAGE e080(sldbv) WITH is_field_sel-field.
    ENDTRY.

    CLEAR ct_where.

    cv_subrc = 1.                  " Keine Abgrenzungen
    IF is_field_sel-options IS INITIAL.
      RETURN.
    ENDIF.

    cv_subrc = 0.

    l_info-ilength = is_field_sel-convert-where_leng.
    DATA(lf_first) = abap_true.

    LOOP AT is_field_sel-options ASSIGNING <ls_option> WHERE sign = 'I'.

      IF lf_first = abap_false.

        where_single_word_new( EXPORTING iv_word        = 'OR'
                                         iv_word_length = 2
                               CHANGING  ct_where       = ct_where
                                         cv_where       = lv_where
                                         cv_offset      = lv_and_offset ).
      ELSE.
        where_single_word_new( EXPORTING iv_word        = '('
                                         iv_word_length = 1
                               CHANGING  ct_where       = ct_where
                                         cv_where       = lv_where
                                         cv_offset      = lv_and_offset ).

        CLEAR lf_first.
      ENDIF.

      single_clause_new( EXPORTING iv_fieldname        = is_field_sel-sqlfieldname
                                   iv_sql_function     = is_field_sel-sql_function
                                   is_option           = <ls_option>
                                   iv_fieldname_length = lv_fieldname_length
                         CHANGING  ct_where            = ct_where
                                   cv_where            = lv_where
                                   cv_offset           = lv_and_offset ).
      l_i_num = l_i_num + 1.
    ENDLOOP.

    IF l_i_num > 0.
      where_single_word_new( EXPORTING iv_word        = ')'
                                       iv_word_length = 1
                             CHANGING  ct_where       = ct_where
                                       cv_where       = lv_where
                                       cv_offset      = lv_and_offset ).
    ENDIF.

    lf_first = abap_true.

    LOOP AT is_field_sel-options ASSIGNING <ls_option> WHERE sign = 'E'.
      IF lf_first = abap_false OR l_i_num > 0.
        where_single_word_new( EXPORTING iv_word        = 'AND'
                                         iv_word_length = 3
                               CHANGING  ct_where       = ct_where
                                         cv_where       = lv_where
                                         cv_offset      = lv_and_offset ).
      ENDIF.
      CLEAR lf_first.
      single_clause_new( EXPORTING iv_fieldname        = is_field_sel-sqlfieldname
                                   iv_sql_function     = is_field_sel-sql_function
                                   is_option           = <ls_option>
                                   iv_fieldname_length = lv_fieldname_length
                         CHANGING  ct_where            = ct_where
                                   cv_where            = lv_where
                                   cv_offset           = lv_and_offset ).
    ENDLOOP.

    ct_where = VALUE #( BASE ct_where ( lv_where ) ).
  ENDMETHOD.

  METHOD where_single_word_new.
    DATA lv_remaining TYPE i.
    FIELD-SYMBOLS <lv_new_word> TYPE any.

    lv_remaining = c_line_length - cv_offset.

    IF iv_word_length < lv_remaining.
      ASSIGN cv_where+cv_offset(iv_word_length) TO <lv_new_word>.
      <lv_new_word> = iv_word.
      cv_offset = cv_offset + iv_word_length + 1.  " Add blank character
    ELSE.
      ct_where = VALUE #( BASE ct_where ( cv_where ) ).
      cv_where = iv_word.
      cv_offset = iv_word_length + 1. " Add blank character
    ENDIF.
  ENDMETHOD.

  METHOD start_new_line.
    CHECK cv_where IS NOT INITIAL.

    ct_where = VALUE #( BASE ct_where ( cv_where ) ).
    cv_offset = 4.
    CLEAR cv_where.
  ENDMETHOD.

  METHOD add_fieldname_to_clause.
    DATA lv_fieldname LIKE iv_fieldname.

    lv_fieldname = iv_fieldname.

    IF iv_sql_function IS NOT INITIAL.

      CASE iv_sql_function.

        WHEN zif_sat_c_sql_function=>upper.
          lv_fieldname = |UPPER( { iv_fieldname } )|.

          " Convert low/high to upper case
          IF cv_low IS NOT INITIAL.
            TRANSLATE cv_low TO UPPER CASE.
          ENDIF.

          IF cv_high IS NOT INITIAL.
            TRANSLATE cv_high TO UPPER CASE.
          ENDIF.

      ENDCASE.
    ENDIF.

    where_single_word_new( EXPORTING iv_word        = CONV #( lv_fieldname )
                                     iv_word_length = strlen( lv_fieldname )
                           CHANGING  ct_where       = ct_where
                                     cv_where       = cv_where
                                     cv_offset      = cv_offset ).
  ENDMETHOD.

  METHOD single_subquery_clause_new.
    " TODO: parameter IV_FIELDNAME_LENGTH is never used (ABAP cleaner)

    DATA lt_subquery TYPE string_table.

    " Always start a new row for a subquery clause
    start_new_line( CHANGING ct_where  = ct_where
                             cv_where  = cv_where
                             cv_offset = cv_offset ).

    IF    iv_option = zif_sat_c_options=>not_in_subquery
       OR iv_option = zif_sat_c_options=>in_subquery.

      add_fieldname_to_clause( EXPORTING iv_fieldname = iv_fieldname
                               CHANGING  ct_where     = ct_where
                                         cv_where     = cv_where
                                         cv_offset    = cv_offset ).

      IF iv_option = zif_sat_c_options=>not_in_subquery.

        where_single_word_new( EXPORTING iv_word        = 'NOT'
                                         iv_word_length = 3
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
      ENDIF.
      where_single_word_new( EXPORTING iv_word        = 'IN'
                                       iv_word_length = 2
                             CHANGING  ct_where       = ct_where
                                       cv_where       = cv_where
                                       cv_offset      = cv_offset ).
      " Wrap subquery in parenthesis
      where_single_word_new( EXPORTING iv_word        = '('
                                       iv_word_length = 1
                             CHANGING  ct_where       = ct_where
                                       cv_where       = cv_where
                                       cv_offset      = cv_offset ).
      " Add subquery clause
      SPLIT iv_subquery AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_subquery.
      LOOP AT lt_subquery ASSIGNING FIELD-SYMBOL(<lv_query_line>).
        start_new_line( CHANGING ct_where  = ct_where
                                 cv_where  = cv_where
                                 cv_offset = cv_offset  ).
        where_single_word_new( EXPORTING iv_word        = <lv_query_line>
                                         iv_word_length = strlen( <lv_query_line> )
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
      ENDLOOP.
      " Close the subquery clause
      where_single_word_new( EXPORTING iv_word        = ')'
                                       iv_word_length = 1
                             CHANGING  ct_where       = ct_where
                                       cv_where       = cv_where
                                       cv_offset      = cv_offset ).

    ELSE.
      " Exists is not supported yet
    ENDIF.
  ENDMETHOD.

  METHOD single_clause_new.
    DATA lv_low TYPE ty_filter_value.
    DATA lv_high TYPE ty_filter_value.
    DATA lv_option TYPE ddoption.
    DATA lv_lowlength TYPE i.
    DATA lv_highlength TYPE i.
    DATA lv_int TYPE i.
    DATA lf_flag TYPE abap_bool.
    DATA lv_offset TYPE i.

    DATA: BEGIN OF ls_escape,
            quote_1 TYPE c LENGTH 1 VALUE '''',
            escape  TYPE c LENGTH 1,
            quote_2 TYPE c LENGTH 1 VALUE '''',
          END   OF ls_escape.

    FIELD-SYMBOLS <l_f> TYPE c.

    " Handle some special options
    CASE is_option-option.

      WHEN zif_sat_c_options=>not_in_subquery OR
           zif_sat_c_options=>in_subquery OR
           zif_sat_c_options=>not_exists_subquery OR
           zif_sat_c_options=>exists_subquery.

        single_subquery_clause_new( EXPORTING iv_fieldname        = iv_fieldname
                                              iv_subquery         = is_option-subquery
                                              iv_option           = is_option-option
                                              iv_fieldname_length = iv_fieldname_length
                                    CHANGING  ct_where            = ct_where
                                              cv_where            = cv_where
                                              cv_offset           = cv_offset ).
        RETURN.
    ENDCASE.

    " always use the original length of literals to prevent trailing spaces
    lv_lowlength = strlen( is_option-low ).
    IF NOT lv_lowlength > 0.
      lv_lowlength = 1.
    ENDIF.

    lv_highlength = strlen( is_option-high ).  " P_INFO-ILENGTH.
    IF NOT lv_highlength > 0.
      lv_highlength = 1.
    ENDIF.

    lv_low = ''''.
    lv_low+1 = is_option-low(lv_lowlength).

    lv_offset = 1.

    DO lv_lowlength TIMES.
      ASSIGN lv_low+lv_offset(*) TO <l_f>.
      IF <l_f> CA ''''.
        lv_offset = lv_offset + sy-fdpos + 2.
        REPLACE '''' WITH '''''' INTO <l_f>.
        lf_flag = abap_true.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF lf_flag = abap_true.
      lv_int = strlen( lv_low+1 ).
      IF lv_int > lv_lowlength.
        lv_lowlength = lv_int.
      ENDIF.
      lf_flag = abap_false.
    ENDIF.

    lv_int = 1 + lv_lowlength.
    lv_low+lv_int = ''''.
    lv_lowlength = lv_lowlength + 2.

    IF    is_option-option = zif_sat_c_options=>between
       OR is_option-option = zif_sat_c_options=>not_between.
      lv_high = ''''.
      lv_high+1 = is_option-high(lv_highlength).

      lv_offset = 1.
      DO lv_highlength TIMES.
        ASSIGN lv_high+lv_offset(*) TO <l_f>.
        IF <l_f> CA ''''.
          lv_offset = lv_offset + sy-fdpos + 2.
          REPLACE '''' WITH '''''' INTO <l_f>.
          lf_flag = abap_true.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      IF lf_flag = abap_true.
        lv_int = strlen( lv_high+1 ).
        IF lv_int > lv_highlength.
          lv_highlength = lv_int.
        ENDIF.
        lf_flag = abap_false.
      ENDIF.

      lv_int = 1 + lv_highlength.
      lv_high+lv_int = ''''.
      lv_highlength = lv_highlength + 2.
    ENDIF.

    lv_option = is_option-option.

    IF is_option-sign = zif_sat_c_options=>excluding.
      CASE lv_option.
        WHEN zif_sat_c_options=>equals.
          lv_option = zif_sat_c_options=>not_equals.
        WHEN zif_sat_c_options=>not_equals.
          lv_option = zif_sat_c_options=>equals.
        WHEN zif_sat_c_options=>between.
          lv_option = zif_sat_c_options=>not_between.
        WHEN zif_sat_c_options=>lesser_equal.
          lv_option = '> '.
        WHEN zif_sat_c_options=>greater_equal.
          lv_option = '< '.
        WHEN zif_sat_c_options=>lesser_than.
          lv_option = zif_sat_c_options=>greater_equal.
        WHEN zif_sat_c_options=>greater_than.
          lv_option = zif_sat_c_options=>lesser_equal.
        WHEN zif_sat_c_options=>contains_pattern.
          lv_option = zif_sat_c_options=>not_contains_pattern.
        WHEN zif_sat_c_options=>not_contains_pattern.
          lv_option = zif_sat_c_options=>contains_pattern.
        WHEN zif_sat_c_options=>not_between.
          lv_option = zif_sat_c_options=>between.
      ENDCASE.
    ELSE.
      CASE lv_option.
        WHEN zif_sat_c_options=>lesser_than.
          lv_option = '< '.
        WHEN zif_sat_c_options=>greater_than.
          lv_option = '> '.
      ENDCASE.
    ENDIF.

    add_fieldname_to_clause( EXPORTING iv_fieldname    = iv_fieldname
                                       iv_sql_function = iv_sql_function
                             CHANGING  ct_where        = ct_where
                                       cv_where        = cv_where
                                       cv_offset       = cv_offset
                                       cv_low          = lv_low
                                       cv_high         = lv_high ).

    DATA lv_sql_pattern TYPE string.
    DATA lf_escape_needed TYPE abap_bool.

    " TODO: If Between is active comparator and sql function is supplied
    " it has to be converted to the following pattern:
    " => [not (] function( field) >= value and function( field ) <= value [)]

    CASE lv_option.

      " Between Option -> low and high have to be filled
      WHEN zif_sat_c_options=>between OR
           zif_sat_c_options=>not_between.

        " Negate the option
        IF lv_option = zif_sat_c_options=>not_between.
          where_single_word_new( EXPORTING iv_word        = 'NOT'
                                           iv_word_length = 3
                                 CHANGING  ct_where       = ct_where
                                           cv_where       = cv_where
                                           cv_offset      = cv_offset ).
        ENDIF.

        where_single_word_new( EXPORTING iv_word        = 'BETWEEN'
                                         iv_word_length = 7
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
        where_single_word_new( EXPORTING iv_word        = CONV #( lv_low )
                                         iv_word_length = lv_lowlength
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
        where_single_word_new( EXPORTING iv_word        = 'AND'
                                         iv_word_length = 3
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
        where_single_word_new( EXPORTING iv_word        = CONV #( lv_high )
                                         iv_word_length = lv_highlength
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).

      " Compare Pattern option i.e. '*word*'
      WHEN zif_sat_c_options=>contains_pattern OR
           zif_sat_c_options=>not_contains_pattern.

        " Negate the option
        IF lv_option = zif_sat_c_options=>not_contains_pattern.
          where_single_word_new( EXPORTING iv_word        = 'NOT'
                                           iv_word_length = 3
                                 CHANGING  ct_where       = ct_where
                                           cv_where       = cv_where
                                           cv_offset      = cv_offset ).
        ENDIF.

        where_single_word_new( EXPORTING iv_word        = 'LIKE'
                                         iv_word_length = 4
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).

        zcl_sat_like_pattern_convrter=>conv_sap_to_sql_pattern( EXPORTING  iv_sap_pattern   = lv_low
                                                                IMPORTING  ev_sql_pattern   = lv_sql_pattern
                                                                           ef_escape_needed = lf_escape_needed
                                                                EXCEPTIONS closing_escape   = 1 ).
        IF sy-subrc = 0.
          lv_low = lv_sql_pattern.
        ENDIF.

        lv_lowlength = strlen( lv_low ).

        IF lf_escape_needed = abap_true.
          ls_escape-escape = '#'.
        ENDIF.

        where_single_word_new( EXPORTING iv_word        = CONV #( lv_low )
                                         iv_word_length = lv_lowlength
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).

        IF ls_escape-escape <> space.
          where_single_word_new( EXPORTING iv_word        = 'ESCAPE'
                                           iv_word_length = 6
                                 CHANGING  ct_where       = ct_where
                                           cv_where       = cv_where
                                           cv_offset      = cv_offset ).
          where_single_word_new( EXPORTING iv_word        = CONV #( ls_escape )
                                           iv_word_length = 3
                                 CHANGING  ct_where       = ct_where
                                           cv_where       = cv_where
                                           cv_offset      = cv_offset ).
        ENDIF.
*.... Handle IS NULL/ NOT IS NULL
      WHEN zif_sat_c_options=>is_not_null OR
           zif_sat_c_options=>is_null.

        IF lv_option = zif_sat_c_options=>is_not_null.
          where_single_word_new( EXPORTING iv_word        = 'IS NOT NULL'
                                           iv_word_length = 11
                                 CHANGING  ct_where       = ct_where
                                           cv_where       = cv_where
                                           cv_offset      = cv_offset ).
        ELSE.
          where_single_word_new( EXPORTING iv_word        = 'IS NULL'
                                           iv_word_length = 7
                                 CHANGING  ct_where       = ct_where
                                           cv_where       = cv_where
                                           cv_offset      = cv_offset ).

        ENDIF.
        " The rest of the option can be handled in a simple manner
      WHEN OTHERS.
        where_single_word_new( EXPORTING iv_word        = CONV #( lv_option )
                                         iv_word_length = 2
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
        where_single_word_new( EXPORTING iv_word        = CONV #( lv_low )
                                         iv_word_length = lv_lowlength
                               CHANGING  ct_where       = ct_where
                                         cv_where       = cv_where
                                         cv_offset      = cv_offset ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
