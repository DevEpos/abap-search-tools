"! <p class="shorttext synchronized">Access elements from DDIC Repository</p>
CLASS zcl_sat_ddic_repo_access DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_table_info,
        tabname     TYPE tabname,
        description TYPE ddtext,
      END OF ty_s_table_info,

      ty_t_table_info TYPE STANDARD TABLE OF ty_s_table_info WITH EMPTY KEY,

      BEGIN OF ty_s_table_definition,
        tabname TYPE tabname,
        ddtext  TYPE ddtext,
        is_view TYPE abap_bool,
      END OF ty_s_table_definition,

      ty_t_table_definition TYPE STANDARD TABLE OF ty_s_table_definition WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Retrieve foreign key tables for the given table name</p>
    CLASS-METHODS get_foreign_key_tables
      IMPORTING
        iv_tabname       TYPE tabname
      RETURNING
        VALUE(rt_entity) TYPE zsat_entity_t.

    "! <p class="shorttext synchronized">Retrieve database entity for the given name</p>
    CLASS-METHODS get_entity
      IMPORTING
        iv_entity_id     TYPE zsat_entity_id
      RETURNING
        VALUE(rs_entity) TYPE zsat_entity
      RAISING
        zcx_sat_data_read_error.

    "! <p class="shorttext synchronized">Retrieve entities in search range</p>
    CLASS-METHODS get_entity_by_range
      IMPORTING
        it_entity_range    TYPE zif_sat_ty_global=>ty_t_tabname_range
      RETURNING
        VALUE(rt_entities) TYPE zsat_entity_t.

    "! <p class="shorttext synchronized">Find database tables/views</p>
    CLASS-METHODS find_database_tab_view
      IMPORTING
        iv_package       TYPE devclass                              OPTIONAL
        if_all           TYPE abap_bool                             OPTIONAL
        iv_type          TYPE zsat_entity_type                      OPTIONAL
        iv_tabname       TYPE tabname                               OPTIONAL
        it_tabname_range TYPE zif_sat_ty_global=>ty_t_tabname_range OPTIONAL
      RETURNING
        VALUE(result)    TYPE zsat_entity_t.

    "! <p class="shorttext synchronized">Find base tables for database view</p>
    CLASS-METHODS find_base_tables_of_view
      IMPORTING
        iv_view_name     TYPE tabname
      RETURNING
        VALUE(rt_result) TYPE zsat_entity_t.

    "! <p class="shorttext synchronized">Retrieve table names for a package</p>
    CLASS-METHODS get_tables_for_package
      IMPORTING
        iv_package    TYPE devclass
      RETURNING
        VALUE(result) TYPE ty_t_table_info.

    "! <p class="shorttext synchronized">Retrieve Text table</p>
    CLASS-METHODS get_text_table
      IMPORTING
        iv_tabname               TYPE tabname
      EXPORTING
        VALUE(ev_text_table)     TYPE tabname
        VALUE(ev_text_key_field) TYPE fieldname.

    "! <p class="shorttext synchronized">Retrieve description of domain fix value</p>
    CLASS-METHODS get_domain_fix_value_text
      IMPORTING
        iv_data          TYPE any
      RETURNING
        VALUE(rv_ddtext) TYPE ddtext.

    "! <p class="shorttext synchronized">Retrieve information about table</p>
    CLASS-METHODS get_table_info
      IMPORTING
        iv_tablename   TYPE tabname
      RETURNING
        VALUE(rs_info) TYPE dd02v.

    "! <p class="shorttext synchronized">Retrieve table fields information</p>
    CLASS-METHODS get_table_field_infos
      IMPORTING
        iv_tablename    TYPE tabname
      EXPORTING
        et_table_fields TYPE dfies_table.

    "! <p class="shorttext synchronized">Retrieve information about single table field</p>
    CLASS-METHODS get_table_field_info
      IMPORTING
        iv_tablename    TYPE tabname
        iv_fieldname    TYPE fieldname
      RETURNING
        VALUE(rs_dfies) TYPE dfies.

    "! <p class="shorttext synchronized">Retrieve data element information</p>
    CLASS-METHODS get_data_element
      IMPORTING
        iv_data_element     TYPE rollname
      RETURNING
        VALUE(rs_dtel_info) TYPE dd04v.

    "! <p class="shorttext synchronized">Retrieve internal type of data element</p>
    CLASS-METHODS get_dtel_inttype
      IMPORTING
        iv_data_element         TYPE rollname
      RETURNING
        VALUE(rv_internal_type) TYPE inttype.

    "! <p class="shorttext synchronized">Read description for table/view</p>
    CLASS-METHODS get_table_description
      IMPORTING
        is_table_info         TYPE dd02v
      RETURNING
        VALUE(rv_description) TYPE dd02v-ddtext.

    "! <p class="shorttext synchronized">Retrieve field information for rollname</p>
    CLASS-METHODS get_dfies_info_for_rollname
      IMPORTING
        iv_rollname     TYPE rollname
      RETURNING
        VALUE(rs_dfies) TYPE dfies.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_ddic_repo_access IMPLEMENTATION.
  METHOD get_data_element.
    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING  name          = iv_data_element     " Name des zu lesenden Datenelements
                 langu         = zcl_sat_system_helper=>get_system_language( )
      IMPORTING  dd04v_wa      = rs_dtel_info    " Header des Datenelements
      EXCEPTIONS illegal_input = 1
                 OTHERS        = 2.
  ENDMETHOD.

  METHOD get_dfies_info_for_rollname.
    DATA(lr_elem_descr) = CAST cl_abap_elemdescr(
        cl_abap_elemdescr=>describe_by_name( iv_rollname ) ).
    rs_dfies = lr_elem_descr->get_ddic_field( p_langu = zcl_sat_system_helper=>get_system_language( ) ).
  ENDMETHOD.

  METHOD get_domain_fix_value_text.
*& Description: Returns the text for the given domain fix value
*&---------------------------------------------------------------------*
    CHECK iv_data IS NOT INITIAL.

    DATA(lr_data_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( iv_data ) ).
    lr_data_descr->get_ddic_fixed_values( EXPORTING  p_langu        = zcl_sat_system_helper=>get_system_language( )
                                          RECEIVING  p_fixed_values = DATA(lt_fix_values)
                                          EXCEPTIONS not_found      = 1
                                                     no_ddic_type   = 2
                                                     OTHERS         = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      rv_ddtext = lt_fix_values[ low = CONV #( iv_data ) ]-ddtext.
    ENDIF.
  ENDMETHOD.

  METHOD get_dtel_inttype.
    DATA(lr_dtel_type) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( iv_data_element ) ).
    rv_internal_type = lr_dtel_type->get_ddic_field( )-inttype.
  ENDMETHOD.

  METHOD get_tables_for_package.
    DATA lt_tadir TYPE STANDARD TABLE OF tabname.

    SELECT obj_name INTO TABLE lt_tadir
      FROM tadir
      WHERE pgmid    = 'R3TR'
        AND object   = 'TABL'
        AND devclass = iv_package.

    IF lt_tadir IS INITIAL.
      RETURN.
    ENDIF.

    " only return db tables
    SELECT tabname ddtext AS description INTO CORRESPONDING FIELDS OF TABLE result
      FROM dd02v
      FOR ALL ENTRIES IN lt_tadir
      WHERE tabname = lt_tadir-table_line
        AND tabclass = 'TRANSP'
        AND ddlanguage = sy-langu.
  ENDMETHOD.

  METHOD get_table_field_info.
*& Description: Returns field infos for table field
*&---------------------------------------------------------------------*
    DATA lv_fieldname TYPE dfies-lfieldname.

    lv_fieldname = iv_fieldname.
    " get components for table
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING  tabname        = iv_tablename
                 lfieldname     = lv_fieldname
                 langu          = zcl_sat_system_helper=>get_system_language( )
      IMPORTING  dfies_wa       = rs_dfies
      EXCEPTIONS not_found      = 1
                 internal_error = 2
                 OTHERS         = 3.
  ENDMETHOD.

  METHOD get_table_field_infos.
*& Description: Returns infos for table fields of the specified table
*&---------------------------------------------------------------------*
    CLEAR et_table_fields.

    " get components for table
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING  tabname        = iv_tablename
                 langu          = zcl_sat_system_helper=>get_system_language( )
      TABLES     dfies_tab      = et_table_fields
      EXCEPTIONS not_found      = 1
                 internal_error = 2
                 OTHERS         = 3.
    IF sy-subrc <> 0.
      CLEAR et_table_fields.
    ENDIF.
  ENDMETHOD.

  METHOD get_table_info.
*& Description: Returns infos for the specified table
*&---------------------------------------------------------------------*
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING  name          = iv_tablename
                 langu         = zcl_sat_system_helper=>get_system_language( )
      IMPORTING  dd02v_wa      = rs_info
      EXCEPTIONS illegal_input = 1
                 OTHERS        = 2.
    IF sy-subrc <> 0.
      CLEAR rs_info.
    ELSE.
      IF rs_info-tabclass = 'VIEW' OR rs_info-ddtext IS INITIAL.
        rs_info-ddtext = get_table_description( rs_info ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_text_table.
*& Description: Returns the text table (if one exists) for the given
*& table name
*&---------------------------------------------------------------------*
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING tabname    = iv_tabname
      IMPORTING texttable  = ev_text_table
                checkfield = ev_text_key_field.
  ENDMETHOD.

  METHOD find_database_tab_view.
    DATA lt_db_range TYPE RANGE OF tabname.
    DATA lt_package_range TYPE RANGE OF devclass.

    DATA(lv_max_rows) = COND #( WHEN if_all = abap_true THEN 0 ELSE 50 ).

    IF it_tabname_range IS NOT INITIAL.
      lt_db_range = it_tabname_range.
    ELSEIF iv_tabname IS NOT INITIAL.
      lt_db_range = VALUE #( ( sign = 'I' option = 'CP' low = iv_tabname ) ).
    ELSEIF iv_package IS NOT INITIAL.
      lt_package_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_package ) ).
    ELSE.
      RETURN.
    ENDIF.

    DATA(lv_descr_language) = zcl_sat_system_helper=>get_system_language( ).

    IF iv_type IS NOT INITIAL.

      CASE iv_type.

        WHEN zif_sat_c_entity_type=>table.
          SELECT tablename AS entity_id, tablename AS entity_id_raw, type AS entity_type, description
            FROM zsat_i_databasetable( p_language = @lv_descr_language )
            WHERE developmentpackage IN @lt_package_range
              AND tablename             IN @lt_db_range
          INTO CORRESPONDING FIELDS OF TABLE @result
            UP TO @lv_max_rows ROWS.
        WHEN zif_sat_c_entity_type=>view.
          SELECT viewname AS entity_id, viewname AS entity_id_raw, type AS entity_type, description
            FROM zsat_i_databaseview( p_language = @lv_descr_language )
            WHERE developmentpackage IN @lt_package_range
              AND viewname             IN @lt_db_range
          INTO CORRESPONDING FIELDS OF TABLE @result
            UP TO @lv_max_rows ROWS.
      ENDCASE.
    ELSE.

      SELECT entity AS entity_id, entity AS entity_id_raw, type AS entity_type, description
        FROM zsat_i_databaseentity( p_language = @lv_descr_language )
        WHERE developmentpackage IN @lt_package_range
          AND entity             IN @lt_db_range
          AND type               <> @zif_sat_c_entity_type=>cds_view
      INTO CORRESPONDING FIELDS OF TABLE @result
        UP TO @lv_max_rows ROWS.

    ENDIF.
  ENDMETHOD.

  METHOD find_base_tables_of_view.
    DATA(lv_descr_language) = zcl_sat_system_helper=>get_system_language( ).

    SELECT basetable AS entity_id,
           basetable AS entity_id_raw,
           entitytype AS entity_type,
           ddtext AS description
      FROM zsaticdsbaset AS base
        LEFT OUTER JOIN dd02t AS text ON base~basetable = text~tabname
                                     AND text~ddlanguage = @lv_descr_language
      WHERE ddlview = @iv_view_name
    INTO CORRESPONDING FIELDS OF TABLE @rt_result.
  ENDMETHOD.

  METHOD get_table_description.
    DATA(lv_language) = zcl_sat_system_helper=>get_system_language( ).

    IF is_table_info-tabclass = 'VIEW'.
      SELECT SINGLE ddtext INTO @rv_description
          FROM dd25t
          WHERE viewname = @is_table_info-tabname
            AND ( ddlanguage = @lv_language
               OR ddlanguage = 'EN' ).
    ELSE.
      SELECT SINGLE ddtext INTO @rv_description
        FROM dd02t
        WHERE tabname = @is_table_info-tabname
          AND ( ddlanguage = @lv_language
             OR ddlanguage = 'EN' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_entity.
    DATA(lv_language) = zcl_sat_system_helper=>get_system_language( ).

    SELECT SINGLE entity AS entity_id,
                  entityraw AS entity_id_raw,
                  type AS entity_type,
                  description
      FROM zsat_i_databaseentity( p_language = @lv_language )
      WHERE entity = @iv_entity_id
    INTO CORRESPONDING FIELDS OF @rs_entity.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_sat_data_read_error
        EXPORTING textid = zcx_sat_data_read_error=>db_entity_not_existing
                  msgv1  = |{ iv_entity_id } |.
    ENDIF.
  ENDMETHOD.

  METHOD get_entity_by_range.
    DATA(lv_language) = zcl_sat_system_helper=>get_system_language( ).

    SELECT entity AS entity_id,
           entityraw AS entity_id_raw,
           type AS entity_type,
           description
      FROM zsat_i_databaseentity( p_language = @lv_language )
      WHERE entity IN @it_entity_range
    INTO CORRESPONDING FIELDS OF TABLE @rt_entities.
  ENDMETHOD.

  METHOD get_foreign_key_tables.
    DATA(lv_language) = zcl_sat_system_helper=>get_system_language( ).

    SELECT foreignkeytable AS entity_id,
           foreignkeytable AS entity_id_raw,
           createdby AS created_by,
           developmentpackage AS devclass,
           description,
           'T' AS entity_type
      FROM zsat_i_foreignkeytable( p_language = @lv_language )
      WHERE tablename = @iv_tabname
      ORDER BY foreignkeytable
    INTO CORRESPONDING FIELDS OF TABLE @rt_entity.
  ENDMETHOD.
ENDCLASS.
