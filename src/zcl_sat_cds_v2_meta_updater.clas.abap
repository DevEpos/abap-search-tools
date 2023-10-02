"! <p class="shorttext synchronized">Updates Metadata for CDS Views v2</p>
CLASS zcl_sat_cds_v2_meta_updater DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ddlname_range    TYPE zif_sat_ty_global=>ty_t_ddlname_range OPTIONAL
        force_update     TYPE abap_bool                             OPTIONAL
        write_debug_info TYPE abap_bool                             OPTIONAL.

    "! <p class="shorttext synchronized">Updates Meta Index of CDS Views v2</p>
    METHODS update_index.
    "! <p class="shorttext synchronized">Removes invalid Meta index of CDS Views v2</p>
    METHODS remove_invalid_index.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ddl_map,
        entityname TYPE ddstrucobjname,
        viewname   TYPE viewname,
      END OF ty_ddl_map,

      ty_ddl_maps TYPE HASHED TABLE OF ty_ddl_map WITH UNIQUE KEY entityname.

    DATA ddlname_range TYPE zif_sat_ty_global=>ty_t_ddlname_range.
    DATA write_debug_info TYPE abap_bool.
    DATA force_update TYPE abap_bool.

    METHODS modify_cds_meta_index.

    METHODS store_meta_information
      IMPORTING
        ddls TYPE ddddlsrc.

    "! Replace any occurrences of CDS Views v1 with their corresponding
    "! DDIC SQL View
    METHODS replace_ddic_view_refs
      CHANGING
        !fields TYPE ty_fields.
ENDCLASS.


CLASS zcl_sat_cds_v2_meta_updater IMPLEMENTATION.
  METHOD constructor.
    me->ddlname_range    = ddlname_range.
    me->write_debug_info = write_debug_info.
    me->force_update     = force_update.
  ENDMETHOD.

  METHOD update_index.
    IF write_debug_info = abap_true.
      WRITE / 'Start Update CDS v2 Meta Index...'.
      DATA(timer) = cl_abap_runtime=>create_hr_timer( ).
      timer->get_runtime( ).
    ENDIF.

    modify_cds_meta_index( ).

    IF write_debug_info = abap_true.
      WRITE / |Finished update of CDS v2 Meta Index in { timer->get_runtime( ) / 1000 NUMBER = USER }ms|.
    ENDIF.
  ENDMETHOD.

  METHOD remove_invalid_index.
    DATA deleted_ddls_range TYPE RANGE OF ddlname.

    SELECT 'I' AS sign,
           'EQ' AS option,
           ddlname AS low
      FROM zsatcds2mhead AS index
        LEFT OUTER JOIN tadir AS repo
          ON index~ddlname = repo~obj_name
          AND repo~object = 'DDLS'
      WHERE repo~obj_name IS NULL
         OR repo~delflag = @abap_true
      INTO CORRESPONDING FIELDS OF TABLE @deleted_ddls_range.

    IF sy-subrc = 0.
      DELETE FROM zsatcds2mbtab WHERE ddlname IN @deleted_ddls_range.
      DELETE FROM zsatcds2mfield WHERE ddlname IN @deleted_ddls_range.
      DELETE FROM zsatcds2mhead WHERE ddlname IN @deleted_ddls_range.

      IF write_debug_info = abap_true.
        WRITE / |Removed Meta Index for { lines( deleted_ddls_range ) } deleted entities|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD modify_cds_meta_index.
    SELECT src~*,
           meta~last_generated_on,
           meta~last_generated_at
      FROM ddddlsrc AS src
        LEFT OUTER JOIN zsatcds2mhead AS meta
          ON  src~ddlname = meta~ddlname
      WHERE source_type IN ( 'W', 'X' )
        AND src~ddlname IN @ddlname_range
      INTO TABLE @DATA(ddls_to_update).

    DATA(update_count) = 0.

    LOOP AT ddls_to_update ASSIGNING FIELD-SYMBOL(<ddls>).
      IF force_update = abap_false AND <ddls>-last_generated_on IS NOT INITIAL.
        IF    <ddls>-src-as4date < <ddls>-last_generated_on
           OR (     <ddls>-src-as4date  = <ddls>-last_generated_on
                AND <ddls>-src-as4time <= <ddls>-last_generated_at ).
          DELETE ddls_to_update.
          CONTINUE.
        ENDIF.
      ENDIF.

      store_meta_information( <ddls>-src ).
      IF write_debug_info = abap_true.
        WRITE / | - DDL Source { <ddls>-src-ddlname } processed|.
      ENDIF.
      update_count = update_count + 1.
    ENDLOOP.

    IF write_debug_info = abap_true AND update_count > 0.
      WRITE / |Updated Meta Index of { update_count } CDS v2 Views|.
    ENDIF.
  ENDMETHOD.

  METHOD store_meta_information.
    DATA(parser) = NEW lcl_parser( ddls ).

    parser->parse( IMPORTING basetables = DATA(basetables)
                             fields     = DATA(fields) ).

    replace_ddic_view_refs( CHANGING fields = fields ).

    " ignore client field
    DELETE fields WHERE fieldname = 'MANDT'.

    DELETE FROM zsatcds2mbtab WHERE ddlname = ddls-ddlname.
    DELETE FROM zsatcds2mfield WHERE ddlname = ddls-ddlname.

    DATA(meta_head) = VALUE zsatcds2mhead( ddlname           = ddls-ddlname
                                           last_generated_at = sy-timlo
                                           last_generated_on = sy-datum ).

    MODIFY zsatcds2mhead FROM meta_head.

    INSERT zsatcds2mfield FROM TABLE fields.
    INSERT zsatcds2mbtab FROM TABLE basetables.
  ENDMETHOD.

  METHOD replace_ddic_view_refs.
    DATA cds_v1_ddic_map TYPE ty_ddl_maps.

    CHECK line_exists( fields[ is_cds_v1 = abap_true ] ).

    SELECT
      FROM zsat_i_ddldependency
      FIELDS viewname,
             entityname
      FOR ALL ENTRIES IN @fields
      WHERE entityname = @fields-basetable
      INTO CORRESPONDING FIELDS OF TABLE @cds_v1_ddic_map.

    IF sy-subrc = 0.

      LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>) WHERE is_cds_v1 = abap_true
           GROUP BY <field>-basetable.

        DATA(ddl_map) = REF #( cds_v1_ddic_map[ entityname = <field>-basetable ] ).

        LOOP AT GROUP <field> ASSIGNING FIELD-SYMBOL(<field_group_entry>).
          <field_group_entry>-basetable = ddl_map->viewname.
        ENDLOOP.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
