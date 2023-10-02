"! <p class="shorttext synchronized">Resource for DDIC Repository Access</p>
CLASS zcl_sat_adt_res_ddic_rep_acc DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_access_mode,
        read_columns_of_entity TYPE string VALUE 'getColumns',
        read_path_uri          TYPE string VALUE 'getUriForPaths',
      END OF c_access_mode.

    CONSTANTS:
      BEGIN OF c_filter_keys,
        no_client_columns TYPE string VALUE 'noClientCols',
      END OF c_filter_keys.

    DATA ms_result TYPE zif_sat_ty_adt_types=>ty_entity_field_info_result.
    DATA mf_no_client_columns TYPE abap_bool.
    DATA mt_paths TYPE string_table.
    DATA mv_access_mode TYPE string.
    DATA mv_object_type TYPE zsat_entity_type.
    DATA mv_object_name TYPE string.

    "! <p class="shorttext synchronized">Reads request parameters</p>
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Retrieves columns of requested ddic entity</p>
    METHODS get_columns.
    "! <p class="shorttext synchronized">Reads URIs for list of paths</p>
    METHODS get_uri_in_paths.

    "! <p class="shorttext synchronized">Retrieve object name and type from the given uri</p>
    METHODS get_object_from_uri
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Reads database table columns</p>
    METHODS read_table_columns.
    "! <p class="shorttext synchronized">Reads CDS View columns</p>
    METHODS read_cds_view_columns.

    "! <p class="shorttext synchronized">Retrieves optional list of filters</p>
    METHODS get_filters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.
ENDCLASS.


CLASS zcl_sat_adt_res_ddic_rep_acc IMPLEMENTATION.
  METHOD get.
    get_parameters( request ).

    CASE mv_access_mode.

      WHEN c_access_mode-read_columns_of_entity.
        IF mv_object_name IS INITIAL.
          RAISE EXCEPTION TYPE zcx_sat_adt_ddic_access_error
            EXPORTING textid = zcx_sat_adt_ddic_access_error=>missing_parameter
                      msgv1  = 'uri'.
        ENDIF.

        get_columns( ).

      WHEN c_access_mode-read_path_uri.
        IF mt_paths IS INITIAL.
          RAISE EXCEPTION TYPE zcx_sat_adt_ddic_access_error
            EXPORTING textid = zcx_sat_adt_ddic_access_error=>missing_parameter
                      msgv1  = 'paths'.
        ENDIF.
        get_uri_in_paths( ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_sat_adt_ddic_access_error
          EXPORTING textid = zcx_sat_adt_ddic_access_error=>invalid_access_mode
                    msgv1  = |{ mv_access_mode }|.
    ENDCASE.

    response->set_body_data( content_handler = zcl_sat_adt_ch_factory=>create_field_entity_res_handlr( )
                             data            = ms_result ).
  ENDMETHOD.

  METHOD get_parameters.
    mv_access_mode = zcl_sat_adt_res_util=>get_request_param_value(
                         iv_param_name = zif_sat_c_adt_utils=>c_ddic_repo_access_params-access_mode
                         if_mandatory  = abap_true
                         io_request    = io_request ).
    mt_paths = zcl_sat_adt_res_util=>get_request_param_values(
                   iv_param_name = zif_sat_c_adt_utils=>c_ddic_repo_access_params-paths
                   io_request    = io_request ).
    get_filters( io_request ).
    get_object_from_uri( io_request ).
  ENDMETHOD.

  METHOD get_filters.
    DATA lt_filter_parts TYPE TABLE OF string.

    DATA(lt_filter) = zcl_sat_adt_res_util=>get_request_param_values(
                          iv_param_name = zif_sat_c_adt_utils=>c_ddic_repo_access_params-filters
                          io_request    = io_request ).

    LOOP AT lt_filter INTO DATA(lv_filter).
      CLEAR lt_filter_parts.
      SPLIT lv_filter AT ':' INTO TABLE lt_filter_parts.
      CHECK lt_filter_parts IS NOT INITIAL.

      DATA(lv_key) = lt_filter_parts[ 1 ].
      DATA(lv_value) = VALUE #( lt_filter_parts[ 2 ] OPTIONAL ).

      CASE lv_key.

        WHEN c_filter_keys-no_client_columns.
          mf_no_client_columns = lv_value.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_columns.
    CASE mv_object_type.

      WHEN zif_sat_c_entity_type=>cds_view.
        read_cds_view_columns( ).

      WHEN zif_sat_c_entity_type=>table OR
           zif_sat_c_entity_type=>view.

        read_table_columns( ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_uri_in_paths.
    DATA(lo_path_resolver) = NEW cl_ddic_adt_ddls_path_resolver( ).
    TRY.
        lo_path_resolver->resolve(
          EXPORTING i_paths            = mt_paths
                    i_uri_required     = abap_true
                    i_exact_match      = abap_true
                    i_completion_scope = cl_ddic_adt_res_ddl_rep_access=>co_completion_scope_all
          IMPORTING r_result           = DATA(lt_result) ).
        LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
          ms_result-field_infos = VALUE #( BASE ms_result-field_infos
                                           ( field       = <ls_result>-name
                                             entity_name = <ls_result>-path
                                             description = <ls_result>-description
                                             type        = <ls_result>-type
                                             uri         = <ls_result>-uri ) ).
        ENDLOOP.
      CATCH cx_ris_exception.
      CATCH cx_adt_uri_mapping.
    ENDTRY.
  ENDMETHOD.

  METHOD get_object_from_uri.
    DATA(lv_uri) = zcl_sat_adt_res_util=>get_request_param_value(
                       iv_param_name = zif_sat_c_adt_utils=>c_ddic_repo_access_params-uri
                       io_request    = io_request ).

    IF lv_uri IS INITIAL.
      RETURN.
    ENDIF.

    zcl_sat_adt_util=>map_uri_to_wb_object( EXPORTING iv_uri         = lv_uri
                                            IMPORTING ev_object_name = mv_object_name
                                                      es_object_type = DATA(ls_object_type) ).

    IF ls_object_type-objtype_tr = 'DDLS'.
      mv_object_type = zif_sat_c_entity_type=>cds_view.
    ELSEIF ls_object_type-objtype_tr = 'TABL' AND ls_object_type-subtype_wb = 'DT'.
      mv_object_type = zif_sat_c_entity_type=>table.
    ELSEIF ls_object_type-objtype_tr = 'VIEW'.
      mv_object_type = zif_sat_c_entity_type=>view.

      " fallback if view is generated ddic sql view of ddls
      SELECT SINGLE ddlname
        FROM zsat_i_ddldependency
        WHERE viewname = @mv_object_name
        INTO @DATA(lv_ddlname_for_view).
      IF sy-subrc = 0.
        mv_object_name = lv_ddlname_for_view.
        mv_object_type = zif_sat_c_entity_type=>cds_view.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_sat_adt_ddic_access_error
        EXPORTING textid = zcx_sat_adt_ddic_access_error=>object_type_not_supported
                  msgv1  = |{ ls_object_type-objtype_tr }/{ ls_object_type-subtype_wb }|.
    ENDIF.
  ENDMETHOD.

  METHOD read_table_columns.
    zcl_sat_ddic_repo_access=>get_table_field_infos( EXPORTING iv_tablename    = CONV #( mv_object_name )
                                                     IMPORTING et_table_fields = DATA(lt_fields) ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF <ls_field>-datatype = 'CLNT'.
        CHECK mf_no_client_columns = abap_false.
      ENDIF.

      DATA(ls_field) = VALUE zif_sat_ty_adt_types=>ty_entity_field_info( field       = <ls_field>-fieldname
                                                                         description = <ls_field>-fieldtext
                                                                         entity_name = mv_object_name
                                                                         is_key      = <ls_field>-keyflag  ).
      ms_result-field_infos = VALUE #( BASE ms_result-field_infos ( ls_field ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD read_cds_view_columns.
    SELECT SINGLE entityid
      FROM zsat_p_cdsviewbase
      WHERE entityid = @mv_object_name
         OR ddlname  = @mv_object_name
    INTO  @DATA(lv_cds_view_name).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        DATA(lo_cds_view) = zcl_sat_cds_view_factory=>read_cds_view( iv_cds_view = lv_cds_view_name ).
        DATA(lt_fields) = lo_cds_view->get_columns( ).

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
          IF <ls_field>-datatype = 'CLNT'.
            CHECK mf_no_client_columns = abap_false.
          ENDIF.

          DATA(ls_field) = VALUE zif_sat_ty_adt_types=>ty_entity_field_info(
                                     field       = <ls_field>-fieldname_raw
                                     description = COND #( WHEN <ls_field>-fieldlabel IS NOT INITIAL
                                                           THEN <ls_field>-fieldlabel
                                                           ELSE <ls_field>-ddtext )
                                     entity_name = lv_cds_view_name
                                     is_key      = <ls_field>-keyflag ).
          ms_result-field_infos = VALUE #( BASE ms_result-field_infos ( ls_field ) ).
        ENDLOOP.

      CATCH zcx_sat_data_read_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
