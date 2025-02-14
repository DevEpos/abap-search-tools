"! <p class="shorttext synchronized">Resolves CDS Field Hierarchy</p>
"! This class can be used to resolve the complete hierarchy
"! of a specific field of a CDS view. The hierarchy will be return in a deep
"! structure
CLASS zcl_sat_cds_field_hier_res DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor.

    "! <p class="shorttext synchronized">CLASS CONSTRUCTOR</p>
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized">Resolve field hierarchy of the given CDS field</p>
    METHODS resolve_field_hierarchy
      IMPORTING
        iv_cds_view         TYPE zsat_cds_view_name
        iv_cds_view_field   TYPE fieldname
      RETURNING
        VALUE(rs_hierarchy) TYPE zif_sat_ty_adt_types=>ty_entity_field_info_result.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_cached_node,
             entity    TYPE tabname,
             field_ref TYPE REF TO lcl_field,
           END OF ty_s_cached_node.
    TYPES: BEGIN OF ty_s_hierarchy_field,
             viewname       TYPE tabname,
             viewfield      TYPE fieldname,
             ddlname        TYPE ddlname,
             level          TYPE i,
             entityname     TYPE tabname,
             viewfieldraw   TYPE fieldname,
             basetable      TYPE tabname,
             baseddlname    TYPE ddlname,
             basesourcetype TYPE zsat_cds_source_type,
             baseentityname TYPE tabname,
             basefield      TYPE fieldname,
             basefieldraw   TYPE fieldname,
           END OF ty_s_hierarchy_field,
           ty_t_hierarchy_field TYPE STANDARD TABLE OF ty_s_hierarchy_field WITH EMPTY KEY.

    DATA mt_cached_nodes TYPE STANDARD TABLE OF ty_s_cached_node.
    DATA mo_path_resolver TYPE REF TO cl_ddic_adt_ddls_path_resolver.

    "! <p class="shorttext synchronized">Retrieve field hierarchy</p>
    METHODS get_field_hierarchy
      IMPORTING
        iv_cds_view              TYPE zsat_cds_view_name
        iv_cds_view_field        TYPE fieldname
      RETURNING
        VALUE(rt_hierarchy_flat) TYPE ty_t_hierarchy_field.

    "! <p class="shorttext synchronized">Retrieve all child nodes for the given view field</p>
    METHODS get_children
      IMPORTING
        iv_viewname  TYPE tabname
        iv_fieldname TYPE fieldname
        it_hierarchy TYPE ty_t_hierarchy_field
        io_field     TYPE REF TO lcl_field.

    METHODS convert_hier_to_adt_result
      IMPORTING
        io_field      TYPE REF TO lcl_field
      RETURNING
        VALUE(result) TYPE zif_sat_ty_adt_types=>ty_entity_field_info_result.

    "! <p class="shorttext synchronized">Converts field information to element info</p>
    METHODS convert_node_to_field_info
      IMPORTING
        io_field     TYPE REF TO lcl_field
      CHANGING
        cs_elem_info TYPE zif_sat_ty_adt_types=>ty_entity_field_info.

    "! <p class="shorttext synchronized">Fills element info from field</p>
    METHODS fill_element_info_from_field
      IMPORTING
        io_field     TYPE REF TO lcl_field
      CHANGING
        cs_elem_info TYPE zif_sat_ty_adt_types=>ty_entity_field_info.

    "! <p class="shorttext synchronized">Fills entity type information of fields</p>
    METHODS fill_type_information.
ENDCLASS.


CLASS zcl_sat_cds_field_hier_res IMPLEMENTATION.
  METHOD constructor.
    mo_path_resolver = NEW cl_ddic_adt_ddls_path_resolver( ).
  ENDMETHOD.

  METHOD class_constructor.
  ENDMETHOD.

  METHOD resolve_field_hierarchy.
    DATA(lt_hierarchy_flat) = get_field_hierarchy( iv_cds_view       = to_upper( iv_cds_view )
                                                   iv_cds_view_field = to_upper( iv_cds_view_field ) ).

    IF lt_hierarchy_flat IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_base_row) = lt_hierarchy_flat[ 1 ].

    DATA(lo_root_field) = NEW lcl_field( ).

    lo_root_field->field            = ls_base_row-viewfield.
    lo_root_field->raw_field        = ls_base_row-viewfieldraw.
    lo_root_field->view_name        = ls_base_row-viewname.
    lo_root_field->view_raw_name    = ls_base_row-entityname.
    lo_root_field->secondary_entity = ls_base_row-ddlname.
    lo_root_field->adt_type         = zif_sat_c_object_types=>data_definition.
    lo_root_field->uri              = zcl_sat_adt_util=>create_adt_uri(
                                          iv_name2 = CONV #( ls_base_row-ddlname )
                                          iv_type  = zif_sat_c_entity_type=>cds_view )-uri.

    get_children( iv_viewname  = ls_base_row-viewname
                  iv_fieldname = ls_base_row-viewfield
                  it_hierarchy = lt_hierarchy_flat
                  io_field     = lo_root_field ).

    fill_type_information( ).

    " Convert result into element info structure for ADT resource
    rs_hierarchy = convert_hier_to_adt_result( lo_root_field ).
  ENDMETHOD.

  METHOD convert_hier_to_adt_result.
    DATA lr_field_info TYPE REF TO zif_sat_ty_adt_types=>ty_entity_field_info.

    result-source_field-field         = io_field->field.
    result-source_field-is_calculated = io_field->is_calculated.

    LOOP AT io_field->children INTO DATA(lo_child).
      APPEND INITIAL LINE TO result-field_infos REFERENCE INTO lr_field_info.

      convert_node_to_field_info( EXPORTING io_field     = lo_child
                                  CHANGING  cs_elem_info = lr_field_info->* ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_field_hierarchy.
    DATA lt_view_fields TYPE STANDARD TABLE OF ty_s_hierarchy_field.

    SELECT DISTINCT field~viewname  AS basetable,
                    field~viewfield AS basefield
      FROM dd27s AS field
           INNER JOIN zsat_p_cdsviewbase AS view_base
             ON  view_base~viewname = field~viewname
             AND view_base~entityid = @iv_cds_view
      WHERE field~viewfield = @iv_cds_view_field
      INTO TABLE @DATA(lt_field_tables).

    WHILE lines( lt_field_tables ) > 0.
      SELECT DISTINCT field~viewname  AS viewname,
                      field~viewfield AS viewfield,
                      field~tabname   AS basetable,
                      field~fieldname AS basefield
        FROM dd27s AS field
        FOR ALL ENTRIES IN @lt_field_tables
        WHERE field~rollname  <> 'MANDT'
          AND field~viewname   = @lt_field_tables-basetable
          AND field~viewfield  = @lt_field_tables-basefield
        INTO CORRESPONDING FIELDS OF TABLE @lt_view_fields.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      LOOP AT lt_view_fields ASSIGNING FIELD-SYMBOL(<ls_view_field>).
        <ls_view_field>-level = 1.
      ENDLOOP.

      IF rt_hierarchy_flat IS INITIAL.
        rt_hierarchy_flat = lt_view_fields.
      ELSE.
        LOOP AT lt_view_fields ASSIGNING <ls_view_field>.
          ASSIGN rt_hierarchy_flat[ basetable = <ls_view_field>-viewname
                                    basefield = <ls_view_field>-viewfield ] TO FIELD-SYMBOL(<ls_root_field>).
          CHECK sy-subrc = 0.

          APPEND INITIAL LINE TO rt_hierarchy_flat ASSIGNING FIELD-SYMBOL(<new_view_field>).
          <new_view_field> = <ls_view_field>.
          <new_view_field>-level = <ls_root_field>-level + 1.
        ENDLOOP.
      ENDIF.

      " TODO: try to only include new base fields that are not already in the result hierarchy
      lt_field_tables = CORRESPONDING #( lt_view_fields ).
      SORT lt_field_tables BY basetable
                              basefield.
      DELETE ADJACENT DUPLICATES FROM lt_field_tables COMPARING basetable basefield.
    ENDWHILE.

    SELECT DISTINCT view~viewname,
                    view~rawentityid,
                    view~ddlname,
                    view~sourcetype
      FROM zsat_p_cdsviewbase AS view
      FOR ALL ENTRIES IN @rt_hierarchy_flat
      WHERE view~viewname = @rt_hierarchy_flat-viewname
         OR view~viewname = @rt_hierarchy_flat-basetable
         OR view~entityid = @rt_hierarchy_flat-basetable
      INTO TABLE @DATA(lt_additional_entity_infos).

    " if additional view information could not be found it makes no sense to
    " also look for additional field information
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT DISTINCT view~viewname,
                    view~rawentityid,
                    view~ddlname,
                    view_field~rawfieldname,
                    view_field~fieldname
      FROM zsat_p_cdsviewbase AS view
           INNER JOIN zsat_i_cdsviewfield AS view_field
             ON view~entityid = view_field~entityid
      FOR ALL ENTRIES IN @rt_hierarchy_flat
      WHERE (    view~viewname        = @rt_hierarchy_flat-viewname
              OR view~viewname        = @rt_hierarchy_flat-basetable
              OR view~entityid        = @rt_hierarchy_flat-basetable )
        AND (    view_field~fieldname = @rt_hierarchy_flat-viewfield
              OR view_field~fieldname = @rt_hierarchy_flat-basefield )
      INTO TABLE @DATA(lt_additional_field_infos).

    LOOP AT lt_additional_entity_infos ASSIGNING FIELD-SYMBOL(<ls_additional_entity_info>).

      LOOP AT rt_hierarchy_flat ASSIGNING <ls_view_field> WHERE    viewname  = <ls_additional_entity_info>-viewname
                                                                OR basetable = <ls_additional_entity_info>-viewname.
        IF <ls_view_field>-viewname = <ls_additional_entity_info>-viewname.
          <ls_view_field>-entityname = <ls_additional_entity_info>-rawentityid.
          <ls_view_field>-ddlname    = <ls_additional_entity_info>-ddlname.
        ENDIF.
        IF <ls_view_field>-basetable = <ls_additional_entity_info>-viewname.
          <ls_view_field>-baseentityname = <ls_additional_entity_info>-rawentityid.
          <ls_view_field>-baseddlname    = <ls_additional_entity_info>-ddlname.
          <ls_view_field>-basesourcetype = <ls_additional_entity_info>-sourcetype.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_additional_field_infos ASSIGNING FIELD-SYMBOL(<ls_additional_field_info>).

      LOOP AT rt_hierarchy_flat ASSIGNING <ls_view_field> WHERE    (     viewname  = <ls_additional_field_info>-viewname
                                                                     AND viewfield = <ls_additional_field_info>-fieldname )
                                                                OR (     basetable = <ls_additional_field_info>-viewname
                                                                     AND basefield = <ls_additional_field_info>-fieldname ).
        IF     <ls_view_field>-viewname  = <ls_additional_field_info>-viewname
           AND <ls_view_field>-viewfield = <ls_additional_field_info>-fieldname.
          <ls_view_field>-viewfieldraw = <ls_additional_entity_info>-rawentityid.
        ENDIF.

        IF     <ls_view_field>-basetable = <ls_additional_field_info>-viewname
           AND <ls_view_field>-basefield = <ls_additional_field_info>-fieldname.
          <ls_view_field>-basefieldraw = <ls_additional_field_info>-rawfieldname.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    SORT rt_hierarchy_flat BY level
                              viewname
                              viewfield
                              basetable
                              basefield.
    DELETE ADJACENT DUPLICATES FROM rt_hierarchy_flat COMPARING level
                                                                viewname
                                                                viewfield
                                                                basetable
                                                                basefield.
  ENDMETHOD.

  METHOD get_children.
    LOOP AT it_hierarchy ASSIGNING FIELD-SYMBOL(<ls_hierarchy>) WHERE     viewname  = iv_viewname
                                                                      AND viewfield = iv_fieldname.

      DATA(lv_base_table) = <ls_hierarchy>-basetable.
      IF lv_base_table IN zcl_sat_cds_view_factory=>gt_helper_ddl_tab_names.
        " Is indication that the field is calculated and a parsing of the owning CDS view
        " is needed to get the origin of the field (e.g. case when ... then view1.field2 else view2.field3 )
        io_field->is_calculated = abap_true.
        RETURN.
      ENDIF.

      DATA(lo_child_field) = NEW lcl_field( ).
      lo_child_field->field            = <ls_hierarchy>-basefield.
      lo_child_field->secondary_entity = <ls_hierarchy>-baseddlname.
      lo_child_field->raw_field        = COND #( WHEN <ls_hierarchy>-basefieldraw IS NOT INITIAL
                                                 THEN <ls_hierarchy>-basefieldraw
                                                 ELSE <ls_hierarchy>-basefield ).
      lo_child_field->view_name        = to_upper( <ls_hierarchy>-basetable ).
      lo_child_field->view_raw_name    = COND #( WHEN <ls_hierarchy>-baseentityname IS NOT INITIAL
                                                 THEN <ls_hierarchy>-baseentityname
                                                 ELSE <ls_hierarchy>-basetable ).
      lo_child_field->source_type      = <ls_hierarchy>-basesourcetype.

      " Recursive call to get all the sub children
      get_children( iv_viewname  = <ls_hierarchy>-basetable
                    iv_fieldname = <ls_hierarchy>-basefield
                    it_hierarchy = it_hierarchy
                    io_field     = lo_child_field ).

      CHECK lo_child_field IS NOT INITIAL.

      " Add field to the children of the current hierarchy level
      IF io_field->children IS INITIAL.
        io_field->children = VALUE #( ).
      ENDIF.

      io_field->children = VALUE #( BASE io_field->children ( lo_child_field ) ).
      mt_cached_nodes = VALUE #( BASE mt_cached_nodes
                                 ( entity = to_upper( lo_child_field->view_raw_name ) field_ref = lo_child_field ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_node_to_field_info.
    FIELD-SYMBOLS <lt_child_field_infos> TYPE zif_sat_ty_adt_types=>ty_entity_field_infos.

    fill_element_info_from_field( EXPORTING io_field = io_field CHANGING cs_elem_info = cs_elem_info ).

    IF io_field->children IS NOT INITIAL.
      cs_elem_info-children = NEW zif_sat_ty_adt_types=>ty_entity_field_infos( ).
      ASSIGN cs_elem_info-children->* TO <lt_child_field_infos>.
    ENDIF.

    LOOP AT io_field->children INTO DATA(lo_child_node).
      APPEND INITIAL LINE TO <lt_child_field_infos> REFERENCE INTO DATA(lr_child_field_info).
      convert_node_to_field_info( EXPORTING io_field     = lo_child_node
                                  CHANGING  cs_elem_info = lr_child_field_info->* ).
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_element_info_from_field.
    cs_elem_info-entity_name     = COND #( WHEN io_field->secondary_entity IS NOT INITIAL
                                           THEN io_field->secondary_entity
                                           ELSE io_field->view_name ).
    cs_elem_info-type            = io_field->adt_type.
    cs_elem_info-uri             = io_field->uri.
    cs_elem_info-alt_entity_name = io_field->view_raw_name.

    cs_elem_info-field           = io_field->raw_field.
    cs_elem_info-source_type     = io_field->source_type.
    cs_elem_info-is_calculated   = io_field->is_calculated.
  ENDMETHOD.

  METHOD fill_type_information.
    DATA lt_entity TYPE RANGE OF zsat_entity_id.

    CHECK mt_cached_nodes IS NOT INITIAL.

    lt_entity = VALUE #( FOR field IN mt_cached_nodes ( sign = 'I' option = 'EQ' low = field-entity ) ).

    SELECT * FROM zsat_i_databaseentitywotext
      WHERE entity IN @lt_entity
      INTO TABLE @DATA(lt_entity_type).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_entity_type ASSIGNING FIELD-SYMBOL(<ls_entity_type>).

      LOOP AT mt_cached_nodes ASSIGNING FIELD-SYMBOL(<ls_cached_node>) WHERE entity = <ls_entity_type>-entity.
        " Fill the URI
        DATA(ls_adt_obj_ref) = zcl_sat_adt_util=>create_adt_uri(
                                   iv_type  = <ls_entity_type>-type
                                   iv_name  = CONV #( <ls_cached_node>-field_ref->view_name )
                                   iv_name2 = CONV #( <ls_cached_node>-field_ref->secondary_entity ) ).
        <ls_cached_node>-field_ref->uri      = ls_adt_obj_ref-uri.
        <ls_cached_node>-field_ref->adt_type = ls_adt_obj_ref-type.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
