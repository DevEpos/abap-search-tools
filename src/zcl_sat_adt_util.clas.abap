"! <p class="shorttext synchronized" lang="en">Utilities for ADT navigation</p>
CLASS zcl_sat_adt_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Create ADT URI for the given entity</p>
    CLASS-METHODS create_adt_uri
      IMPORTING
        iv_type                    TYPE zsat_entity_type OPTIONAL
        iv_tadir_type              TYPE tadir-object OPTIONAL
        iv_name                    TYPE zsat_entity_id OPTIONAL
        iv_name2                   TYPE zsat_entity_id OPTIONAL
      RETURNING
        VALUE(rs_object_reference) TYPE sadt_object_reference.
    "! <p class="shorttext synchronized" lang="en">Open Object with ADT Tools</p>
    CLASS-METHODS jump_adt
      IMPORTING
        iv_obj_name     TYPE tadir-obj_name
        iv_obj_type     TYPE tadir-object
        iv_sub_obj_name TYPE tadir-obj_name OPTIONAL
        iv_sub_obj_type TYPE tadir-object OPTIONAL
        iv_line_number  TYPE i OPTIONAL
      RAISING
        zcx_sat_adt_error .
    "! <p class="shorttext synchronized" lang="en">Retrieve adt object and names</p>
    CLASS-METHODS get_adt_objects_and_names
      IMPORTING
        iv_obj_name       TYPE tadir-obj_name
        iv_obj_type       TYPE tadir-object
      EXPORTING
        er_adt_uri_mapper TYPE REF TO if_adt_uri_mapper
        er_adt_objectref  TYPE REF TO cl_adt_object_reference
        ev_program        TYPE progname
        ev_include        TYPE progname
      RAISING
        zcx_sat_adt_error .
    "! <p class="shorttext synchronized" lang="en">Retrieve ADT Object URI for the given name/type</p>
    CLASS-METHODS get_adt_object_ref_uri
      IMPORTING
        iv_name       TYPE seu_objkey
        is_type       TYPE wbobjtype
      RETURNING
        VALUE(rv_uri) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Maps the given URI to a workbench object</p>
    CLASS-METHODS map_uri_to_wb_object
      IMPORTING
        iv_uri                TYPE string
      EXPORTING
        VALUE(ev_object_name) TYPE string
        VALUE(es_object_type) TYPE wbobjtype
      RAISING
        cx_adt_uri_mapping.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_adt_object_uri_cache,
        name TYPE seu_objkey,
        type TYPE wbobjtype,
        uri  TYPE string,
      END OF ty_s_adt_object_uri_cache.
    TYPES:
      BEGIN OF ty_s_adt_object_ref_cache,
        name       TYPE sobj_name,
        type       TYPE trobjtype,
        adt_object TYPE sadt_object_reference,
      END OF ty_s_adt_object_ref_cache.
    CLASS-DATA gt_adt_obj_ref_uri_map TYPE HASHED TABLE OF ty_s_adt_object_uri_cache WITH UNIQUE KEY name type.
    CLASS-DATA gt_adt_obj_ref_map TYPE HASHED TABLE OF ty_s_adt_object_ref_cache WITH UNIQUE KEY name type.
    "! <p class="shorttext synchronized" lang="en">Check if jump is possible for given object</p>
    CLASS-METHODS is_adt_jump_possible
      IMPORTING
        !ir_wb_object                  TYPE REF TO cl_wb_object
        !ir_adt                        TYPE REF TO if_adt_tools_core_factory
      RETURNING
        VALUE(rf_is_adt_jump_possible) TYPE abap_bool
      RAISING
        zcx_sat_adt_error .

ENDCLASS.



CLASS zcl_sat_adt_util IMPLEMENTATION.


  METHOD get_adt_objects_and_names.
    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    cl_wb_object=>create_from_transport_key(
      EXPORTING
        p_object    = lv_obj_type
        p_obj_name  = lv_obj_name
      RECEIVING
        p_wb_object = DATA(lr_wb_object)
      EXCEPTIONS
        OTHERS      = 1 ).
    IF sy-subrc <> 0.
      zcx_sat_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDIF.

    DATA(lr_adt_tools) = cl_adt_tools_core_factory=>get_instance( ).

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = lr_wb_object
      RECEIVING
        p_wb_request      = DATA(lr_wb_request)
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_sat_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDIF.

    DATA(lo_vit_adt_mapper) = lr_adt_tools->get_uri_mapper_vit( ).

    IF lo_vit_adt_mapper->is_vit_wb_request( lr_wb_request ).
      er_adt_objectref = lo_vit_adt_mapper->map_wb_request_to_objref( wb_request = lr_wb_request ).
    ELSE.
      er_adt_uri_mapper = lr_adt_tools->get_uri_mapper( ).

      er_adt_objectref = er_adt_uri_mapper->map_wb_object_to_objref(
          wb_object          = lr_wb_object
      ).

      IF ev_program IS SUPPLIED.
        er_adt_uri_mapper->map_objref_to_include(
          EXPORTING
            uri                = er_adt_objectref->ref_data-uri
          IMPORTING
            program            = ev_program
            include            = ev_include
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_adt_jump_possible.
    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = ir_wb_object
      RECEIVING
        p_wb_request      = DATA(lr_wb_request)
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_sat_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDIF.

    TRY.
        DATA(lr_adt_uri_mapper_vit) = ir_adt->get_uri_mapper_vit( ).
        rf_is_adt_jump_possible = xsdbool( NOT lr_adt_uri_mapper_vit->is_vit_wb_request( wb_request = lr_wb_request ) ).
      CATCH cx_root.
        zcx_sat_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_adt_object_ref_uri.
    ASSIGN gt_adt_obj_ref_uri_map[ name = iv_name type = is_type ] TO FIELD-SYMBOL(<ls_uri>).
    IF sy-subrc <> 0.
      DATA(lo_uri_mapper) = cl_adt_uri_mapper=>get_instance( ).
      TRY.
          rv_uri = lo_uri_mapper->if_adt_uri_mapper~get_adt_object_ref_uri(
              name = iv_name
              type = is_type
          ).
        CATCH cx_adt_uri_mapping.
      ENDTRY.
      INSERT VALUE #(
        name = iv_name
        type = is_type
        uri  = rv_uri
      ) INTO TABLE gt_adt_obj_ref_uri_map.
    ELSE.
      rv_uri = <ls_uri>-uri.
    ENDIF.

  ENDMETHOD.


  METHOD jump_adt.
    get_adt_objects_and_names(
      EXPORTING
        iv_obj_name        = iv_obj_name
        iv_obj_type        = iv_obj_type
      IMPORTING
        er_adt_uri_mapper = DATA(lr_adt_uri_mapper)
        er_adt_objectref  = DATA(lr_adt_objref)
        ev_program        = DATA(lv_program)
        ev_include        = DATA(lv_include)
    ).

    TRY.
        IF iv_sub_obj_name IS NOT INITIAL.

          IF ( lv_program <> iv_obj_name AND lv_include IS INITIAL ) OR
             ( lv_program = lv_include AND iv_sub_obj_name IS NOT INITIAL ).
            lv_include = iv_sub_obj_name.
          ENDIF.

          DATA(lr_adt_sub_objref) = lr_adt_uri_mapper->map_include_to_objref(
              program            = lv_program
              include            = lv_include
              line               = iv_line_number
              line_offset        = 0
              end_line           = iv_line_number
              end_offset         = 1
          ).

          IF lr_adt_sub_objref IS NOT INITIAL.
            lr_adt_objref = lr_adt_sub_objref.
          ENDIF.

        ENDIF.

        DATA(lv_adt_link) = |adt://{ sy-sysid }{ lr_adt_objref->ref_data-uri }|.

        cl_gui_frontend_services=>execute( EXPORTING  document = lv_adt_link
                                           EXCEPTIONS OTHERS   = 1 ).

        IF sy-subrc <> 0.
          zcx_sat_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
        ENDIF.

      CATCH cx_root.
        zcx_sat_adt_error=>raise_adt_error_with_text( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD create_adt_uri.
    DATA: lv_obj_type LIKE iv_tadir_type.

    IF ( iv_name IS INITIAL AND
         iv_name2 IS INITIAL ) OR
       ( iv_type IS INITIAL AND
         iv_tadir_type IS INITIAL ).
      CLEAR rs_object_reference.
      RETURN.
    ENDIF.

    IF iv_type IS SUPPLIED.
      lv_obj_type = SWITCH trobjtype(
        iv_type
        WHEN zif_sat_c_entity_type=>cds_view THEN 'DDLS'
        WHEN zif_sat_c_entity_type=>view THEN 'VIEW'
        WHEN zif_sat_c_entity_type=>table THEN 'TABD'
      ).
    ELSEIF iv_tadir_type IS SUPPLIED.
      lv_obj_type = iv_tadir_type.
    ENDIF.

    DATA(lv_name) = SWITCH sobj_name(
      iv_type
      WHEN zif_sat_c_entity_type=>cds_view THEN iv_name2
      ELSE                                       iv_name
    ).

    ASSIGN gt_adt_obj_ref_map[ name = lv_name type = lv_obj_type ] TO FIELD-SYMBOL(<ls_adt_object>).
    IF sy-subrc <> 0.

      TRY.
          zcl_sat_adt_util=>get_adt_objects_and_names(
            EXPORTING
              iv_obj_name       = lv_name
              iv_obj_type       = lv_obj_type
            IMPORTING
              er_adt_objectref  = DATA(lr_adt_objectref)
          ).
          rs_object_reference = lr_adt_objectref->ref_data.
        CATCH zcx_sat_adt_error.
      ENDTRY.

      INSERT VALUE #(
        name       = lv_name
        type       = lv_obj_type
        adt_object = rs_object_reference
      ) INTO TABLE gt_adt_obj_ref_map.
    ELSE.
      rs_object_reference = <ls_adt_object>-adt_object.
    ENDIF.

  ENDMETHOD.

  METHOD map_uri_to_wb_object.
    CHECK iv_uri IS NOT INITIAL.

    DATA(lo_adt_tools_core_f) = cl_adt_tools_core_factory=>get_instance( ).
    DATA(lo_wb_object) = lo_adt_tools_core_f->get_uri_mapper( )->map_objref_to_wb_object(
        uri             = iv_uri
    ).
    ev_object_name = lo_wb_object->get_display_name( ).
    es_object_type = lo_wb_object->get_object_type_ref( )->get_transport_type( ).
  ENDMETHOD.

ENDCLASS.
