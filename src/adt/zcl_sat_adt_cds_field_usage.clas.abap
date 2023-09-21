"! <p class="shorttext synchronized" lang="en">Usage analysis of CDS field</p>
CLASS zcl_sat_adt_cds_field_usage DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_cds_view TYPE zsat_entity_id
        iv_field    TYPE fieldname.
    "! <p class="shorttext synchronized" lang="en">Retrieve usages in calculated fields</p>
    METHODS get_usages_in_calc_fields
      RETURNING
        VALUE(rt_usages) TYPE zif_sat_ty_adt_types=>ty_t_field_usage.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_cds_usage,
        entityid    TYPE zsat_entity_id,
        rawentityid TYPE zsat_entity_id,
        viewfield   TYPE fieldname,
        ddlname     TYPE ddlname,
        sourcetype  TYPE char1,
      END OF ty_s_cds_usage.

    TYPES:
      BEGIN OF ty_s_field_usage,
        field TYPE string,
      END OF ty_s_field_usage.
    TYPES: ty_t_field_usage TYPE STANDARD TABLE OF ty_s_field_usage WITH EMPTY KEY.

    DATA: mv_cds_view        TYPE zsat_entity_id,
          mt_cds_usages      TYPE STANDARD TABLE OF ty_s_cds_usage,
          mv_field           TYPE fieldname,
          mt_usages          TYPE zif_sat_ty_adt_types=>ty_t_field_usage,
          mt_field_usage_raw TYPE STANDARD TABLE OF ty_s_field_usage.

    "! <p class="shorttext synchronized" lang="en">Reads usages of the the given CDS view</p>
    METHODS read_cds_usages
      RETURNING
        VALUE(rf_usages_found) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Parse CDS views to find field usages</p>
    METHODS parse_view
      IMPORTING
        is_cds_view TYPE ty_s_cds_usage
        it_fields   TYPE ty_t_field_usage.
ENDCLASS.



CLASS zcl_sat_adt_cds_field_usage IMPLEMENTATION.

  METHOD constructor.
    mv_cds_view = to_upper( iv_cds_view ).
    mv_field = to_upper( iv_field ).
  ENDMETHOD.

  METHOD get_usages_in_calc_fields.
    CHECK read_cds_usages( ).

    LOOP AT mt_cds_usages ASSIGNING FIELD-SYMBOL(<ls_cds_usage>)
      GROUP BY <ls_cds_usage>-entityid.

      parse_view( is_cds_view = <ls_cds_usage>
                  it_fields   = VALUE #( FOR field IN GROUP <ls_cds_usage>
                                         ( field  = field-viewfield ) ) ).
    ENDLOOP.

    rt_usages = mt_usages.
  ENDMETHOD.

  METHOD read_cds_usages.
*.. Select all possible CDS views which have at least one calculated field
*.. and the "mv_cds_view"-view as a data source
    SELECT base~entityid,
           base~rawentityid,
           base~ddlname,
           base~sourcetype,
           field~viewfield
      FROM zsaticdsfpe AS frompart
        INNER JOIN zsatpcdsvb AS base
          ON base~viewname = frompart~ddlviewname
        INNER JOIN dd27s AS field
          ON field~viewname = base~viewname
          AND ( field~tabname LIKE 'DDDDL%TYPES' OR
                field~tabname = 'DDDDLNUM_DUMMY' )
      WHERE frompart~sourceentity = @mv_cds_view
    INTO CORRESPONDING FIELDS OF TABLE @mt_cds_usages.

    rf_usages_found = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD parse_view.

    SELECT SINGLE *
        FROM ddddlsrc
        WHERE ddlname = @is_cds_view-ddlname
      INTO @DATA(ls_cds).

    CHECK sy-subrc = 0.

    DATA(lo_parser) = NEW cl_ddl_parser( ).
    TRY.
        DATA(lo_stmnt) = lo_parser->parse_ddl(
            source  = ls_cds-source
        ).
      CATCH cx_ddl_parser_exception.
        RETURN.
    ENDTRY.

    DATA(lo_visitor) = NEW lcl_field_visitor(
      iv_source_entityname = |{ mv_cds_view }|
      iv_source_field      = |{ mv_field }|
    ).
    IF lo_stmnt IS NOT BOUND.
      RETURN.
    ENDIF.

    CHECK lo_stmnt->get_type( ) = cl_qlast_constants=>ddlstmt_type_view_definition.

    DATA(lo_view_stmnt) = CAST cl_qlast_view_definition( lo_stmnt ).
    DATA(lo_select_list) = lo_view_stmnt->get_select( )->get_selectlist( ).

    TRY.
        LOOP AT it_fields INTO DATA(ls_field).
          lo_visitor->mv_current_field = ls_field-field.
          lo_select_list->accept( lo_visitor ).
        ENDLOOP.
        DATA(lt_fields) = lo_visitor->get_found_fields( ).

        IF lt_fields IS NOT INITIAL.

*........ Select raw field names
          SELECT fieldname,
                 rawfieldname
             FROM zsat_i_cdsviewfield
             FOR ALL ENTRIES IN @lt_fields
             WHERE entityid = @is_cds_view-entityid
               AND fieldname = @lt_fields-table_line
          INTO TABLE @DATA(lt_field_names).
        ENDIF.

        LOOP AT lt_fields INTO DATA(lv_field).
          DATA(ls_field_usage) = VALUE zif_sat_ty_adt_types=>ty_s_field_usage(
              entityid      = is_cds_view-rawentityid
              ddlname       = is_cds_view-ddlname
              fieldname     = VALUE #( lt_field_names[ fieldname = lv_field ]-rawfieldname DEFAULT lv_field )
              sourcetype    = is_cds_view-sourcetype
              is_calculated = abap_true
          ).
          mt_usages = VALUE #( BASE mt_usages ( ls_field_usage ) ).
        ENDLOOP.

      CATCH cx_ddl_visitor_exception.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
