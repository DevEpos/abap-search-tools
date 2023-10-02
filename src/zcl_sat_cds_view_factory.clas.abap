"! <p class="shorttext synchronized">Factory for Accessing CDS View information</p>
CLASS zcl_sat_cds_view_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Tables for calculated fields in CDS view</p>
    CLASS-DATA gt_helper_ddl_tab_names TYPE RANGE OF tabname READ-ONLY.

    "! <p class="shorttext synchronized">Retrieves position of entity name in ddl source</p>
    CLASS-METHODS get_entityname_pos_in_ddlsrc
      IMPORTING
        iv_entity_id TYPE zsat_entity_id
        iv_source    TYPE string
      EXPORTING
        ev_column    TYPE i
        ev_row       TYPE i.

    "! <p class="shorttext synchronized">Checks if the given CDS view was recently changed</p>
    CLASS-METHODS has_cds_view_changed
      IMPORTING
        iv_cds_view_name     TYPE zsat_cds_view_name
        iv_last_changed_date TYPE d
        iv_last_changed_time TYPE t
      RETURNING
        VALUE(rf_changed)    TYPE abap_bool.

    "! <p class="shorttext synchronized">Activate CDS View</p>
    CLASS-METHODS activate_cds_view
      IMPORTING
        iv_cds_view TYPE ddlname
      RAISING
        zcx_sat_application_exc.

    "! <p class="shorttext synchronized">Checks Syntax of CDS View</p>
    CLASS-METHODS check_cds_view
      IMPORTING
        iv_cds_view TYPE ddlname
        iv_source   TYPE string
      RAISING
        zcx_sat_application_exc.

    "! <p class="shorttext synchronized">CLASS_CONSTRUCTOR</p>
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized">Checks if the given cds view exists</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized"></p>
    "! @parameter result           | <p class="shorttext synchronized"></p>
    CLASS-METHODS exists
      IMPORTING
        iv_cds_view_name TYPE zsat_cds_view_name
      RETURNING
        VALUE(result)    TYPE abap_bool.

    "! <p class="shorttext synchronized">Retrieve annotation/value for a range of annotation names</p>
    "!
    CLASS-METHODS get_annotations
      IMPORTING
        it_annotation_name TYPE zif_sat_ty_global=>ty_t_cds_anno_name_range
      RETURNING
        VALUE(rt_anno)     TYPE zif_sat_ty_global=>ty_t_cds_annotation.

    "! <p class="shorttext synchronized">Finds header information for cds view(s)</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized"></p>
    "! @parameter iv_description   | <p class="shorttext synchronized"></p>
    "! @parameter iv_package       | <p class="shorttext synchronized"></p>
    "! @parameter iv_max_rows      | <p class="shorttext synchronized"></p>
    "! @parameter result           | <p class="shorttext synchronized"></p>
    CLASS-METHODS find_cds_views
      IMPORTING
        iv_cds_view_name TYPE zsat_cds_view_name OPTIONAL
        iv_description   TYPE ddtext             OPTIONAL
        iv_package       TYPE devclass           OPTIONAL
        iv_max_rows      TYPE sy-tabix           DEFAULT 50
      RETURNING
        VALUE(result)    TYPE zsat_entity_t.

    "! <p class="shorttext synchronized">Read API states for Cds View</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized"></p>
    CLASS-METHODS get_api_states
      IMPORTING
        iv_cds_view          TYPE zsat_cds_view_name
      RETURNING
        VALUE(rt_api_states) TYPE zif_sat_ty_global=>ty_t_cds_api_state.

    "! <p class="shorttext synchronized">Get author of given CDS View</p>
    "!
    "! @parameter iv_cds_view | <p class="shorttext synchronized"></p>
    "! @parameter rs_tadir    | <p class="shorttext synchronized">Repository information for CDS view</p>
    CLASS-METHODS get_tadir_entry
      IMPORTING
        iv_cds_view     TYPE zsat_cds_view_name
      RETURNING
        VALUE(rs_tadir) TYPE zif_sat_ty_global=>ty_s_cds_tadir.

    "! <p class="shorttext synchronized">Retrieve DDL Name for CDS Entity name</p>
    "!
    "! @parameter iv_cds_view_name | <p class="shorttext synchronized"></p>
    "! @parameter rv_ddl_name      | <p class="shorttext synchronized"></p>
    CLASS-METHODS get_ddl_for_entity_name
      IMPORTING
        iv_cds_view_name   TYPE zsat_cds_view_name
      RETURNING
        VALUE(rv_ddl_name) TYPE ddlname
      RAISING
        zcx_sat_data_read_error.

    "! <p class="shorttext synchronized">Get CDS view for package</p>
    "!
    "! @parameter iv_package | <p class="shorttext synchronized"></p>
    "! @parameter result     | <p class="shorttext synchronized"></p>
    CLASS-METHODS get_ddl_for_package
      IMPORTING
        iv_package    TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_sat_ty_global=>ty_t_cds_header.

    "! <p class="shorttext synchronized">Get description of cds view</p>
    "!
    "! @parameter iv_cds_view    | <p class="shorttext synchronized"></p>
    "! @parameter rv_description | <p class="shorttext synchronized"></p>
    CLASS-METHODS get_description
      IMPORTING
        iv_cds_view           TYPE zsat_cds_view_name
      RETURNING
        VALUE(rv_description) TYPE ddtext.

    "! <p class="shorttext synchronized">Reads CDS View reference</p>
    "!
    "! @parameter iv_cds_view             | <p class="shorttext synchronized"></p>
    "! @parameter result                  | <p class="shorttext synchronized"></p>
    "! @raising   zcx_sat_data_read_error | <p class="shorttext synchronized"></p>
    CLASS-METHODS read_cds_view
      IMPORTING
        iv_cds_view   TYPE zsat_cds_view_name
      RETURNING
        VALUE(result) TYPE REF TO zcl_sat_cds_view
      RAISING
        zcx_sat_data_read_error.

    "! <p class="shorttext synchronized">Reads head information of cds view</p>
    CLASS-METHODS read_cds_view_header
      IMPORTING
        iv_cds_view   TYPE zsat_cds_view_name
      RETURNING
        VALUE(result) TYPE zsat_cds_view_header.

    "! <p class="shorttext synchronized">Reads multiple headers for cds views</p>
    CLASS-METHODS read_cds_view_header_multi
      IMPORTING
        it_cds_view_name TYPE zif_sat_ty_global=>ty_t_cds_view_name
      RETURNING
        VALUE(result)    TYPE dd02bvtab.

    "! <p class="shorttext synchronized">Reads the source of the CDS View</p>
    CLASS-METHODS read_ddls_source
      IMPORTING
        iv_cds_view_name TYPE zsat_cds_view_name
      RETURNING
        VALUE(rv_source) TYPE string
      RAISING
        zcx_sat_application_exc.

    "! <p class="shorttext synchronized">Reads DDIC SQL View for the given entity</p>
    CLASS-METHODS read_ddl_ddic_view_for_entity
      IMPORTING
        iv_entity_id            TYPE zsat_entity_id
      RETURNING
        VALUE(rv_ddic_sql_view) TYPE string.

    "! <p class="shorttext synchronized">Read DDIC View for CDS View</p>
    CLASS-METHODS read_ddl_ddic_view
      IMPORTING
        iv_ddl_name         TYPE ddlname
      RETURNING
        VALUE(rv_ddic_view) TYPE viewname.

    "! <p class="shorttext synchronized">Reads entity name for given DDLS name</p>
    CLASS-METHODS get_entity_name_for_ddls
      IMPORTING
        iv_ddls_name          TYPE ddobjname
      RETURNING
        VALUE(rv_entity_name) TYPE zsat_cds_view_name
      RAISING
        zcx_sat_data_read_error.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF lty_cds_view_cache,
        ddl_view_name TYPE viewname,
        cds_view_name TYPE zsat_cds_view_name,
        ref           TYPE REF TO zcl_sat_cds_view,
        language      TYPE langu,
      END OF lty_cds_view_cache.

    CLASS-DATA st_cds_view_cache TYPE SORTED TABLE OF lty_cds_view_cache WITH UNIQUE KEY cds_view_name language.
    "! <p class="shorttext synchronized">Description of Domain Fixed Values</p>
    CLASS-DATA st_cds_source_vals TYPE ddfixvalues.

    "! <p class="shorttext synchronized">Fills associations read from ddic</p>
    CLASS-METHODS fill_associations
      IMPORTING
        it_header     TYPE dd08bvtab
        it_fields     TYPE dd05bvtab
      RETURNING
        VALUE(result) TYPE zif_sat_ty_global=>ty_t_cds_association.

    "! <p class="shorttext synchronized">Retrieves the source type of a cds view</p>
    "!
    "! @parameter iv_ddl_name    | <p class="shorttext synchronized"></p>
    "! @parameter rv_source_type | <p class="shorttext synchronized"></p>
    CLASS-METHODS get_source_type
      IMPORTING
        iv_ddl_name           TYPE ddlname
      RETURNING
        VALUE(rv_source_type) TYPE zsat_cds_source_type.

    "! <p class="shorttext synchronized">Handler for REQUEST_ANNOTATIONS</p>
    "!
    CLASS-METHODS on_annotation_read_request
      FOR EVENT request_annotations OF zcl_sat_cds_view
      IMPORTING
        et_anno_name_range
        sender.

    "! <p class="shorttext synchronized">Event Handler for lazy loading of CDS View API states</p>
    CLASS-METHODS on_api_states_loading_request
      FOR EVENT request_api_states OF zcl_sat_cds_view
      IMPORTING
        sender.

    "! <p class="shorttext synchronized">Event Handler for lazy loading of CDS View author</p>
    CLASS-METHODS on_tadir_info_loading_request
      FOR EVENT request_tadir_info OF zcl_sat_cds_view
      IMPORTING
        sender.

    "! <p class="shorttext synchronized">Event handler for lazy loading of CDS Views base tables</p>
    CLASS-METHODS on_base_table_loading_request
      FOR EVENT request_base_tables OF zcl_sat_cds_view
      IMPORTING
        sender.

    "! <p class="shorttext synchronized">Event Handler for lazy loading of CDS View description</p>
    CLASS-METHODS on_description_loading_request
      FOR EVENT request_description OF zcl_sat_cds_view
      IMPORTING
        sender.

    "! <p class="shorttext synchronized">Reads base tables of DDL View</p>
    CLASS-METHODS read_cds_base_tables
      IMPORTING
        iv_view_name          TYPE viewname
      RETURNING
        VALUE(rt_base_tables) TYPE zsat_cds_view_base_table_t.

    "! <p class="shorttext synchronized">Reads annotations for parameters</p>
    CLASS-METHODS read_param_annotations
      IMPORTING
        iv_cds_view  TYPE zsat_cds_view_name
      CHANGING
        ct_parameter TYPE zif_sat_ty_global=>ty_t_cds_parameter.

ENDCLASS.


CLASS zcl_sat_cds_view_factory IMPLEMENTATION.
  METHOD activate_cds_view.
    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        lr_ddl->activate( name = iv_cds_view ).
      CATCH cx_dd_ddl_activate INTO DATA(lx_dd_ddl_activate).
        RAISE EXCEPTION TYPE zcx_sat_application_exc
          EXPORTING previous = lx_dd_ddl_activate.
    ENDTRY.
  ENDMETHOD.

  METHOD check_cds_view.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_warnings TYPE ddl2ddicwarnings.
    DATA ls_ddl_source TYPE ddddlsrcv.

    ls_ddl_source-source     = iv_source.
    ls_ddl_source-ddlanguage = sy-langu.
    " .. TODO: Fill other components

    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        lr_ddl->check( EXPORTING name       = iv_cds_view    " Name of a DDL Source
*                                 prid       = " ID for Log Writer
                       IMPORTING warnings   = lt_warnings
                       CHANGING  ddlsrcv_wa = ls_ddl_source ).
      CATCH cx_dd_ddl_check INTO DATA(lx_dd_ddl_check).
        RAISE EXCEPTION TYPE zcx_sat_application_exc
          EXPORTING previous = lx_dd_ddl_check.
    ENDTRY.
  ENDMETHOD.

  METHOD class_constructor.
    DATA(lr_cds_sourc_typedescr) = CAST cl_abap_elemdescr(
      cl_abap_typedescr=>describe_by_data( VALUE zsat_cds_source_type( ) ) ).

    st_cds_source_vals = lr_cds_sourc_typedescr->get_ddic_fixed_values( p_langu = 'E' ).

    gt_helper_ddl_tab_names = VALUE #( LET sign = 'I'
                                           opt  = 'EQ' IN
                                       sign   = sign
                                       option = opt
                                       ( low = 'DDDDLCHARTYPES' )
                                       ( low = 'DDDDLCURRTYPES' )
                                       ( low = 'DDDDLDECTYPES' )
                                       ( low = 'DDDDLNUMTYPES' )
                                       ( low = 'DDDDLNUM_DUMMY' )
                                       ( low = 'DDDDLQUANTYPES' ) ).
  ENDMETHOD.

  METHOD exists.
    SELECT SINGLE *
    FROM dd02b
    " TODO: variable is assigned but never used (ABAP cleaner)
    INTO @DATA(ls_view)
    WHERE strucobjn = @iv_cds_view_name.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD fill_associations.
    DATA lv_and_or_value TYPE vsconj VALUE zif_sat_c_selection_condition=>and.
    DATA lt_db_tables TYPE STANDARD TABLE OF zsat_i_databasetable.
    DATA lv_entity_id_raw TYPE zsat_entity_id_raw.
    DATA lv_description TYPE ddtext.
    DATA lt_db_table_range TYPE RANGE OF tabname.
    DATA lt_assoc_cds_view_range TYPE RANGE OF zsat_cds_view_name.
    DATA lf_no_and_or_field TYPE abap_bool.

    lt_assoc_cds_view_range = VALUE #( FOR <ls_assoc_cds> IN it_header
                                       WHERE ( typekind_t <> zif_sat_c_cds_assoc_type=>table )
                                       ( sign = 'I' option = 'EQ' low = <ls_assoc_cds>-strucobjn_t ) ).
    SORT lt_assoc_cds_view_range.
    DELETE ADJACENT DUPLICATES FROM lt_assoc_cds_view_range.

    " .. Read database table+text if there are any association which point to database tables
    IF line_exists( it_header[ typekind_t = zif_sat_c_cds_assoc_type=>table ] ).
      lt_db_table_range = VALUE #( FOR dbtab IN it_header WHERE ( typekind_t = zif_sat_c_cds_assoc_type=>table )
                                   ( sign = 'I' option = 'EQ' low = dbtab-strucobjn_t ) ).
      SELECT *
          FROM zsat_i_databasetable
          WHERE tablename IN @lt_db_table_range
      INTO CORRESPONDING FIELDS OF TABLE @lt_db_tables.
    ENDIF.

    " ... read cds view texts for associations
    SELECT entityid, rawentityid, description
      FROM zsat_i_cdsentity
      WHERE entityid IN @lt_assoc_cds_view_range
    INTO TABLE @DATA(lt_assoc_cds_view_header).

    LOOP AT it_header ASSIGNING FIELD-SYMBOL(<ls_header>).
      IF <ls_header>-typekind_t = zif_sat_c_cds_assoc_type=>table.
        lv_entity_id_raw = <ls_header>-strucobjn_t.
        DATA(ls_dbtab) = VALUE #( lt_db_tables[ tablename = <ls_header>-strucobjn_t ] OPTIONAL ).
        lv_description = ls_dbtab-description.
      ELSE.
        DATA(ls_assoc_cds) = VALUE #( lt_assoc_cds_view_header[ entityid = <ls_header>-strucobjn_t ] OPTIONAL ).
        lv_entity_id_raw = ls_assoc_cds-rawentityid.
        lv_description = ls_assoc_cds-description.
      ENDIF.

      DATA(ls_assoc) = VALUE zsat_cds_association(
          name             = <ls_header>-associationname
          raw_name         = <ls_header>-assocname_raw
          ref_cds_view     = <ls_header>-strucobjn_t
          ref_cds_view_raw = lv_entity_id_raw
          kind             = <ls_header>-typekind_t
          entity_type      = SWITCH #(
            <ls_header>-typekind_t
            WHEN zif_sat_c_cds_assoc_type=>table OR
                 zif_sat_c_cds_assoc_type=>view
            THEN zif_sat_c_entity_type=>table
            ELSE zif_sat_c_entity_type=>cds_view )
          cardinality      = <ls_header>-cardinality
          cardinality_text = |[{ <ls_header>-card_min }..| &&
                             COND #( WHEN <ls_header>-card_max > 10 THEN |*| ELSE |{ <ls_header>-card_max }| ) &&
                             |]|
          card_min         = <ls_header>-card_min
          card_max         = <ls_header>-card_max
          ddtext           = lv_description ).

      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE associationname = <ls_header>-associationname.

        ASSIGN COMPONENT 'AND_OR' OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<lv_and_or>).
        IF sy-subrc = 0.
          lv_and_or_value = <lv_and_or>.
        ELSE.
          lf_no_and_or_field = abap_true.
        ENDIF.
        ls_assoc-fields = VALUE #( BASE ls_assoc-fields
                                   ( name     = <ls_field>-fieldname_t
                                     ref_name = <ls_field>-fieldname
                                     position = <ls_field>-fdposition
                                     operator = <ls_field>-operator
                                     and_or   = lv_and_or_value ) ).
      ENDLOOP.

      IF lf_no_and_or_field = abap_true.
        CLEAR ls_assoc-fields[ lines( ls_assoc-fields ) ]-and_or.
      ENDIF.

      result = VALUE #( BASE result ( ls_assoc ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_cds_views.
    DATA lt_cds_view_range TYPE RANGE OF ddstrucobjname.
    DATA lt_description_range TYPE RANGE OF ddtext.
    DATA lt_package_range TYPE RANGE OF devclass.

    IF iv_cds_view_name IS NOT INITIAL.
      lt_cds_view_range = VALUE #( ( sign   = 'I'
                                     option = COND #( WHEN iv_cds_view_name CS '*' THEN 'CP' ELSE 'EQ' )
                                     low    = iv_cds_view_name ) ).
    ELSEIF iv_package IS NOT INITIAL.
      lt_package_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_package ) ).
    ENDIF.

    IF iv_description IS NOT INITIAL.
      lt_description_range = VALUE #( ( sign   = 'I'
                                        option = COND #( WHEN iv_description CS '*' THEN 'CP' ELSE 'EQ' )
                                        low    = iv_description ) ).
    ENDIF.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_descr_language) = zcl_sat_system_helper=>get_system_language( ).

    SELECT entityid AS entity_id,
           rawentityid AS entity_id_raw,
           'C' AS entity_type,
           description,
           developmentpackage AS devclass
      FROM zsat_i_cdsentity
      WHERE entityid           IN @lt_cds_view_range
        AND developmentpackage IN @lt_package_range
        AND description        IN @lt_description_range
      ORDER BY entity_id
    INTO CORRESPONDING FIELDS OF TABLE @result
      UP TO @iv_max_rows ROWS.
  ENDMETHOD.

  METHOD get_api_states.
    SELECT filtervalue
     FROM zsat_i_apistates
     WHERE objectname = @iv_cds_view
       AND objecttype = 'DDLS'
     INTO TABLE @rt_api_states.

    IF sy-subrc <> 0.
      rt_api_states = VALUE #( ( zif_sat_c_cds_api_state=>not_released ) ).
    ELSE.

    ENDIF.
  ENDMETHOD.

  METHOD get_tadir_entry.
    SELECT SINGLE author AS created_by, created_on AS created_date, devclass
      FROM tadir
      WHERE object   = 'STOB'
        AND pgmid    = 'R3TR'
        AND obj_name = @iv_cds_view
    INTO @rs_tadir.
  ENDMETHOD.

  METHOD get_annotations.
    SELECT *
      FROM zsat_i_cdsannotation
      WHERE name IN @it_annotation_name
    INTO CORRESPONDING FIELDS OF TABLE @rt_anno.
  ENDMETHOD.

  METHOD get_ddl_for_entity_name.
    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        rv_ddl_name = lr_ddl->get_ddl_name_4_dd_artefact( ddname = iv_cds_view_name ).
      CATCH cx_dd_ddl_exception INTO DATA(lx_ddl_read).
        RAISE EXCEPTION TYPE zcx_sat_data_read_error
          EXPORTING previous = lx_ddl_read.
    ENDTRY.
  ENDMETHOD.

  METHOD get_entity_name_for_ddls.
    SELECT SINGLE objectname
      FROM ddldependency
      WHERE objecttype = 'STOB'
        AND ddlname    = @iv_ddls_name
    INTO @rv_entity_name.
  ENDMETHOD.

  METHOD get_ddl_for_package.
    SELECT obj_name AS ddl
      FROM tadir
        WHERE object = 'STOB'
          AND devclass = @iv_package
    INTO TABLE @DATA(lt_cds_views).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = CORRESPONDING #(
        read_cds_view_header_multi( it_cds_view_name = VALUE #( FOR cds IN lt_cds_views
                                                                ( CONV #( cds-ddl ) ) ) )
        MAPPING entityname = strucobjn
                  description = ddtext ).
  ENDMETHOD.

  METHOD get_description.
    SELECT SINGLE description
      FROM zsat_i_cdsentity
      WHERE entityid = @iv_cds_view
    INTO @rv_description.
  ENDMETHOD.

  METHOD get_source_type.
    DATA(lv_select) = 'SOURCE_TYPE'.
    SELECT SINGLE (lv_select)
      FROM ddddlsrc
      WHERE ddlname = @iv_ddl_name
    INTO @rv_source_type.

    IF sy-subrc <> 0.
      rv_source_type = zif_sat_c_cds_view_type=>view.
    ENDIF.
  ENDMETHOD.

  METHOD on_annotation_read_request.
    SELECT *
      FROM zsat_i_cdsannotation
      WHERE entityid = @sender->mv_view_name
        AND name IN @et_anno_name_range
    APPENDING CORRESPONDING FIELDS OF TABLE @sender->mt_annotations.

    IF sy-subrc = 0.
      SORT sender->mt_annotations BY fieldname
                                     name.
      DELETE ADJACENT DUPLICATES FROM sender->mt_annotations COMPARING fieldname name.
    ENDIF.
  ENDMETHOD.

  METHOD on_api_states_loading_request.
    sender->mt_api_states = get_api_states( sender->mv_view_name ).

    SET HANDLER on_api_states_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.

  METHOD on_tadir_info_loading_request.
    sender->ms_tadir_info = get_tadir_entry( sender->mv_view_name ).

    SET HANDLER on_tadir_info_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.

  METHOD on_base_table_loading_request.
    sender->mt_base_tables = read_cds_base_tables( iv_view_name = sender->ms_header-ddlview  ).

    " ... remove handler for this cds view
    SET HANDLER on_base_table_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.

  METHOD on_description_loading_request.
    sender->mv_description = get_description( sender->mv_view_name ).

    SET HANDLER on_description_loading_request FOR sender ACTIVATION abap_false.
  ENDMETHOD.

  METHOD read_cds_base_tables.
*    CONSTANTS lc_table_type TYPE trobjtype VALUE 'TABL'.
*    CONSTANTS lc_view_type  TYPE trobjtype VALUE 'VIEW'.

    TYPES: BEGIN OF lty_base_table.
             INCLUDE TYPE zsat_cds_view_base_table.
    TYPES:   entitytype     TYPE zsat_entity_type,
             generationflag TYPE genflag.
    TYPES: END OF lty_base_table.

    DATA lt_base_tables TYPE STANDARD TABLE OF lty_base_table.
    DATA lt_cds_view_range TYPE RANGE OF viewname.
    DATA lt_view_range TYPE RANGE OF viewname.
    DATA lt_tab_range TYPE RANGE OF tabname.

    FIELD-SYMBOLS <ls_base> TYPE lty_base_table.

    " .. Select all base tables for the given generated SQL View
    SELECT basetable AS entityname,
           basetable AS original_base_name,
           entitytype,
           generationflag
      FROM zsat_i_cdsbasetable
      WHERE ddlview = @iv_view_name
        AND basetable NOT IN @gt_helper_ddl_tab_names
      ORDER BY tableposition
      INTO CORRESPONDING FIELDS OF TABLE @lt_base_tables.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_tab_range = VALUE #( FOR table IN lt_base_tables
                            WHERE ( entitytype = zif_sat_c_entity_type=>table )
                            ( sign = 'I' option = 'EQ' low = table-entityname ) ).
    lt_cds_view_range = VALUE #( FOR view IN lt_base_tables
                                 WHERE ( generationflag = abap_true )
                                 ( sign = 'I' option = 'EQ' low = view-entityname ) ).
    lt_view_range = VALUE #( FOR view IN lt_base_tables
                             WHERE ( generationflag = abap_false AND entitytype = zif_sat_c_entity_type=>view )
                             ( sign = 'I' option = 'EQ' low = view-entityname ) ).

    " .. Fill additional information for CDS views bases
    IF lt_cds_view_range IS NOT INITIAL.
      SELECT viewname,
             ddlname,
             entityid,
             rawentityid,
             sourcetype,
             description,
             \_apistate-apistate AS apistate
        FROM zsat_i_cdsentity
        WHERE viewname IN @lt_cds_view_range
           OR entityid IN @lt_cds_view_range
      INTO TABLE @DATA(lt_cds_view).

      IF sy-subrc = 0.
        LOOP AT lt_base_tables ASSIGNING <ls_base> WHERE generationflag = abap_true.
          ASSIGN lt_cds_view[ viewname = <ls_base>-entityname ] TO FIELD-SYMBOL(<ls_cds_view>).
          IF sy-subrc <> 0.
            ASSIGN lt_cds_view[ entityid = <ls_base>-entityname ] TO <ls_cds_view>.
          ENDIF.

          CHECK sy-subrc = 0.

          <ls_base>-table_kind          = zif_sat_c_entity_type=>cds_view.
          <ls_base>-entityname          = <ls_cds_view>-entityid.
          <ls_base>-entityname_raw      = <ls_cds_view>-rawentityid.
          <ls_base>-description         = <ls_cds_view>-description.
          <ls_base>-api_state           = <ls_cds_view>-apistate.
          <ls_base>-source_type         = <ls_cds_view>-sourcetype.
          <ls_base>-secondary_entity_id = <ls_cds_view>-ddlname.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " .. Add view information
    IF lt_view_range IS NOT INITIAL.
      SELECT viewname,
             description
        FROM zsat_i_databaseview
        WHERE viewname IN @lt_view_range
      INTO TABLE @DATA(lt_view_data).

      IF sy-subrc = 0.
        LOOP AT lt_base_tables ASSIGNING <ls_base> WHERE     entitytype     = zif_sat_c_entity_type=>view
                                                         AND generationflag = abap_false.
          ASSIGN lt_view_data[ viewname = <ls_base>-entityname ] TO FIELD-SYMBOL(<ls_view_data>).
          CHECK sy-subrc = 0.

          <ls_base>-table_kind     = zif_sat_c_entity_type=>view.
          <ls_base>-entityname_raw = <ls_view_data>-viewname.
          <ls_base>-is_db_view     = abap_true.
          <ls_base>-description    = <ls_view_data>-description.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " .. Add table information
    IF lt_tab_range IS NOT INITIAL.
      SELECT tablename,
             description
        FROM zsat_i_databasetable
        WHERE tablename IN @lt_tab_range
      INTO TABLE @DATA(lt_table_data).

      IF sy-subrc = 0.
        LOOP AT lt_base_tables ASSIGNING <ls_base> WHERE entitytype = zif_sat_c_entity_type=>table.
          ASSIGN lt_table_data[ tablename = <ls_base>-entityname ] TO FIELD-SYMBOL(<ls_table_data>).
          CHECK sy-subrc = 0.

          <ls_base>-table_kind     = zif_sat_c_entity_type=>table.
          <ls_base>-entityname_raw = <ls_table_data>-tablename.
          <ls_base>-description    = <ls_table_data>-description.
        ENDLOOP.
      ENDIF.
    ENDIF.

    rt_base_tables = CORRESPONDING #( lt_base_tables ).
  ENDMETHOD.

  METHOD read_cds_view.
    DATA(lv_description_language) = zcl_sat_system_helper=>get_system_language( ).

    " ... try to read view from cache
    result = VALUE #( st_cds_view_cache[ cds_view_name = iv_cds_view
                                         language      = lv_description_language ]-ref OPTIONAL ).

    IF result IS BOUND.
      " ... Check if the cds view was recently changed
      IF has_cds_view_changed( iv_cds_view_name     = result->mv_view_name
                               iv_last_changed_date = result->ms_header-chgdate
                               iv_last_changed_time = result->ms_header-chgtime ).
        DELETE st_cds_view_cache WHERE     cds_view_name = iv_cds_view
                                       AND language      = lv_description_language.
        CLEAR result.
      ENDIF.
    ENDIF.

    IF result IS BOUND.
      RETURN.
    ENDIF.

    DATA(lr_dd_sobject) = cl_dd_sobject_factory=>create( ).

    TRY.

        lr_dd_sobject->read( EXPORTING get_state    = 'A'
                                       withtext     = abap_true
                                       langu        = lv_description_language
                                       sobjnames    = VALUE #( ( iv_cds_view ) )
                             IMPORTING dd02bv_tab   = DATA(lt_headers)
                                       dd02bndv_tab = DATA(lt_nodes)
                                       dd03ndv_tab  = DATA(lt_col)
                                       dd08bv_tab   = DATA(lt_assoc_header)
                                       dd05bv_tab   = DATA(lt_assoc_fields)
                                       dd10bv_tab   = DATA(lt_params_db) ).

        " entity was not found -> maybe it has been deleted
        IF lt_headers IS INITIAL.
          RAISE EXCEPTION TYPE zcx_sat_data_read_error
            EXPORTING textid = zcx_sat_data_read_error=>cds_view_not_existing
                      msgv1  = |{ iv_cds_view }|.
        ENDIF.

        DATA(ls_dd02bv) = lt_headers[ 1 ].

        DATA(ls_header) = CORRESPONDING zsat_cds_view_header(
          ls_dd02bv
          MAPPING entityname     = strucobjn
                  entityname_raw = strucobjn_raw
                  description    = ddtext ).

        ls_header-ddlname = get_ddl_for_entity_name( ls_header-entityname ).

        " ... supply generated ddl database view of ddls
        " ... and read base tables of cds view ( if view name was supplied )
        ls_header-ddlview = VALUE #( lt_nodes[ 1 ]-dbtabname OPTIONAL ).

        " ... if this cds is an extend view the source type can be determined right here
        IF ls_dd02bv-strucobjclass = 'APPEND'.
          ls_header-source_type = zif_sat_c_cds_view_type=>extend.
        ELSE.
          " ... determine the correct source type of the cds
          ls_header-source_type = get_source_type( iv_ddl_name = ls_header-ddlname ).
        ENDIF.

        IF ls_header-source_type IS NOT INITIAL.
          ls_header-source_type_name = st_cds_source_vals[ low = ls_header-source_type ]-ddtext.
        ENDIF.

        DATA(lt_params) = CORRESPONDING zif_sat_ty_global=>ty_t_cds_parameter(
          lt_params_db
          MAPPING inttype = abaptype ).

        " ...... complete parameters with annotations (if existing)
        read_param_annotations( EXPORTING iv_cds_view  = iv_cds_view
                                CHANGING  ct_parameter = lt_params ).

        IF lt_assoc_header IS NOT INITIAL.
          SORT lt_assoc_fields BY associationname
                                  fdposition.
          DATA(lt_association) = fill_associations( it_header = lt_assoc_header
                                                    it_fields = lt_assoc_fields ).
        ENDIF.

        " ...... Delete client field if existing
        DELETE lt_col WHERE datatype = 'CLNT'.

        result = NEW zcl_sat_cds_view( is_header      = ls_header
                                       it_association = lt_association
                                       it_columns     = lt_col
                                       it_parameters  = lt_params ).

        " ... set handler for lazy loading of base tables
        SET HANDLER on_base_table_loading_request FOR result.
        SET HANDLER on_annotation_read_request FOR result.
        SET HANDLER on_api_states_loading_request FOR result.
        SET HANDLER on_tadir_info_loading_request FOR result.
        SET HANDLER on_description_loading_request FOR result.

        " ... fill cache
        INSERT VALUE #( ddl_view_name = ls_header-ddlview
                        cds_view_name = ls_header-entityname
                        language      = lv_description_language
                        ref           = result )
               INTO TABLE st_cds_view_cache.
      CATCH cx_dd_sobject_get INTO DATA(lx_sobject_get). " Read error
        RAISE EXCEPTION TYPE zcx_sat_data_read_error
          EXPORTING previous = lx_sobject_get.
    ENDTRY.
  ENDMETHOD.

  METHOD read_cds_view_header.
    DATA(lr_dd_util) = cl_dd_sobject_factory=>create_util( ).

    TRY.
        lr_dd_util->get_header( EXPORTING entitynames = VALUE #( ( iv_cds_view ) )
                                          withtext    = abap_true    " ABAP_true: Read texts also
                                          langu       = zcl_sat_system_helper=>get_system_language( )
                                IMPORTING header      = DATA(lt_header) ).
        result = CORRESPONDING #( VALUE #( lt_header[ 1 ] OPTIONAL ) ).
      CATCH cx_dd_sobject_get.
    ENDTRY.
  ENDMETHOD.

  METHOD read_cds_view_header_multi.
    CHECK it_cds_view_name IS NOT INITIAL.

    DATA(lr_dd_sobject) = cl_dd_sobject_factory=>create( ).

    TRY.
        lr_dd_sobject->read( EXPORTING langu      = zcl_sat_system_helper=>get_system_language( )
                                       withtext   = abap_true
                                       sobjnames  = it_cds_view_name
                             IMPORTING dd02bv_tab = result ).
      CATCH cx_dd_sobject_get.
    ENDTRY.
  ENDMETHOD.

  METHOD read_ddls_source.
    DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        DATA(lv_ddl_name) = lr_ddl->get_ddl_name_4_dd_artefact( ddname = iv_cds_view_name ).

        lr_ddl->read( EXPORTING name         = lv_ddl_name
                                get_state    = 'A'    " Version of DDL source to be read
                                langu        = zcl_sat_system_helper=>get_system_language( )
                      IMPORTING ddddlsrcv_wa = DATA(ls_ddddl_source) ).
        rv_source = ls_ddddl_source-source.
      CATCH cx_dd_ddl_exception INTO DATA(lx_ddl_read).
        RAISE EXCEPTION TYPE zcx_sat_application_exc
          EXPORTING previous = lx_ddl_read.
    ENDTRY.
  ENDMETHOD.

  METHOD read_ddl_ddic_view_for_entity.
    TRY.
        DATA(lv_ddl_name) = get_ddl_for_entity_name( iv_entity_id ).
        rv_ddic_sql_view = read_ddl_ddic_view( lv_ddl_name ).
      CATCH zcx_sat_data_read_error.
    ENDTRY.

    IF rv_ddic_sql_view IS INITIAL.
      rv_ddic_sql_view = iv_entity_id.
    ENDIF.
  ENDMETHOD.

  METHOD read_ddl_ddic_view.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lr_dd_sobject) = cl_dd_sobject_factory=>create( ).
    DATA(lr) = cl_dd_ddl_handler_factory=>create( ).
    TRY.
        lr->get_ddl_content_object_names( EXPORTING ddlname   = to_upper( iv_ddl_name )
                                                    get_state = 'A'    " Version of ABAP Dictionary Object
                                          IMPORTING viewname  = rv_ddic_view ). " Name of SQL View Defined in DDL Source
      CATCH cx_dd_ddl_read.
    ENDTRY.
  ENDMETHOD.

  METHOD read_param_annotations.
    TYPES: BEGIN OF ty_param_anno,
             parametername TYPE ddparname,
             name          TYPE c LENGTH 240,
             position      TYPE i,
             value         TYPE zsat_annotation_value,
           END OF ty_param_anno.

    DATA lt_paramanno TYPE STANDARD TABLE OF ty_param_anno WITH EMPTY KEY.

    DATA(lt_select) = VALUE string_table( ( |PARAMETERNAME, | )
                                          ( |NAME, | )
                                          ( |PARPOS as POSITION, | )
                                          ( |VALUE | ) ).
    DATA(lv_from) = 'DDPARAMETERANNO'.
    DATA(lv_order_by) = |POSITION|.
    DATA(lt_where) = VALUE string_table( ( |STRUCOBJN = @iv_cds_view| ) ).

    TRY.
        SELECT (lt_select)
          FROM (lv_from)
          WHERE (lt_where)
          ORDER BY (lv_order_by)
        INTO CORRESPONDING FIELDS OF TABLE @lt_paramanno.
      CATCH cx_sy_open_sql_db INTO DATA(lr_osql_db_exc). " TODO: variable is assigned but never used (ABAP cleaner)
      CATCH cx_sy_dynamic_osql_semantics INTO DATA(lr_osql_semantics_exc). " TODO: variable is assigned but never used (ABAP cleaner)
      CATCH cx_sy_dynamic_osql_syntax INTO DATA(lr_osql_syntax_exc). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.

    " ... enrich parameters with annotation values
    LOOP AT lt_paramanno ASSIGNING FIELD-SYMBOL(<ls_paramanno>)
         GROUP BY ( parameter = <ls_paramanno>-parametername )
         ASSIGNING FIELD-SYMBOL(<ls_anno_group>).

      DATA(lr_s_param) = REF #( ct_parameter[ parametername = <ls_anno_group>-parameter ] OPTIONAL ).
      CHECK lr_s_param IS BOUND.

      LOOP AT GROUP <ls_anno_group> ASSIGNING FIELD-SYMBOL(<ls_anno>).
        lr_s_param->annotations = VALUE #( BASE lr_s_param->annotations
                                           ( CORRESPONDING #( <ls_anno> ) ) ).

        " ...... do some annotation checks
        IF <ls_anno>-name = to_upper( zif_sat_c_cds_anno_definition=>consumption_defaultvalue ).
          DATA(lv_val) = <ls_anno>-value.
          DATA(lv_length) = strlen( <ls_anno>-value ).
          " ....... remove leading single quote
          IF lv_val(1) = ''''.
            lv_length = lv_length - 1.
            lv_val = lv_val+1(lv_length).
          ENDIF.

          " ....... remove trailing single quote
          lv_length = lv_length - 1.
          IF lv_val+lv_length(1) = ''''.
            lv_val = lv_val(lv_length).
          ENDIF.
          lr_s_param->default_value = lv_val.
        ELSEIF <ls_anno>-name = to_upper( zif_sat_c_cds_anno_definition=>environment_systemfield ).
          " ........ Handle system environment annotations
          IF <ls_anno>-value = zif_sat_c_cds_anno_value=>c_environment_system_field-client.
            lr_s_param->has_system_anno = abap_true.
          ELSE.
            CASE <ls_anno>-value.
              WHEN zif_sat_c_cds_anno_value=>c_environment_system_field-date.
                lr_s_param->default_value = sy-datum.
              WHEN zif_sat_c_cds_anno_value=>c_environment_system_field-language.
                lr_s_param->default_value = zcl_sat_system_helper=>get_system_language( ).
              WHEN zif_sat_c_cds_anno_value=>c_environment_system_field-time.
                lr_s_param->default_value = sy-timlo.
              WHEN zif_sat_c_cds_anno_value=>c_environment_system_field-user.
                lr_s_param->default_value = sy-uname.
            ENDCASE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_cds_view_changed.
    DATA(ls_header) = read_cds_view_header( iv_cds_view = iv_cds_view_name ).
    IF ls_header IS INITIAL.
      rf_changed = abap_true.
      RETURN.
    ENDIF.

    rf_changed = xsdbool( iv_last_changed_date < ls_header-chgdate OR iv_last_changed_time < ls_header-chgtime ).
  ENDMETHOD.

  METHOD get_entityname_pos_in_ddlsrc.
    DATA lt_ddls_source_lines TYPE TABLE OF string.

    ev_column = -1.
    ev_row = -1.

    " .. Look for line 'DEFINE (VIEW|TABLE FUNCTION) <ENTITY_NAME>'
    SPLIT iv_source AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_ddls_source_lines.

    LOOP AT lt_ddls_source_lines INTO DATA(lv_line).
      DATA(lv_tabix) = sy-tabix.
      DATA(lv_pos) = find( val = lv_line occ = 1 sub = iv_entity_id case = abap_false ).
      IF lv_pos >= 0 AND lv_line CP '*define*'.
        ev_column = lv_pos.
        ev_row = lv_tabix.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
