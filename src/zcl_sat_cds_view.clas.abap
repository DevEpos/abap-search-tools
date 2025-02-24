"! <p class="shorttext synchronized">CDS View</p>
CLASS zcl_sat_cds_view DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_sat_cds_view_factory.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Name of CDS view</p>
    DATA mv_view_name TYPE zsat_cds_view_name READ-ONLY.
    DATA mv_association_count TYPE i READ-ONLY.
    DATA mv_select_table_count TYPE i READ-ONLY.

    "! <p class="shorttext synchronized">Requests annotations for CDS View</p>
    EVENTS request_annotations
      EXPORTING
        VALUE(et_anno_name_range) TYPE zif_sat_ty_global=>ty_t_cds_anno_name_range.
    "! <p class="shorttext synchronized">Requests API States</p>
    EVENTS request_api_states.
    "! <p class="shorttext synchronized">Requests author of CDS View</p>
    EVENTS request_tadir_info.
    "! <p class="shorttext synchronized">Requests Base Tables</p>
    EVENTS request_base_tables.
    "! <p class="shorttext synchronized">Requests Description</p>
    EVENTS request_description.

    METHODS constructor
      IMPORTING
        is_header      TYPE zsat_cds_view_header
        it_columns     TYPE dd03ndvtab
        it_parameters  TYPE zif_sat_ty_global=>ty_t_cds_parameter
        it_association TYPE zif_sat_ty_global=>ty_t_cds_association OPTIONAL
        it_api_states  TYPE zif_sat_ty_global=>ty_t_cds_api_state   OPTIONAL
        it_base_tables TYPE zsat_cds_view_base_table_t              OPTIONAL.

    "! <p class="shorttext synchronized">Get Annotation of CDS View</p>
    "!
    METHODS get_annotations
      IMPORTING
        it_annotation_name   TYPE zif_sat_ty_global=>ty_t_cds_anno_name_range
      RETURNING
        VALUE(rt_annotation) TYPE zif_sat_ty_global=>ty_t_cds_annotation.

    "! <p class="shorttext synchronized">Retrieve API States of CDS View</p>
    "!
    "! @parameter rt_api_states | <p class="shorttext synchronized"></p>
    METHODS get_api_states
      RETURNING
        VALUE(rt_api_states) TYPE zif_sat_ty_global=>ty_t_cds_api_state.

    "! <p class="shorttext synchronized">Returns associations</p>
    METHODS get_associations
      RETURNING
        VALUE(result) TYPE zif_sat_ty_global=>ty_t_cds_association.

    "! <p class="shorttext synchronized">Get author of CDS view</p>
    "!
    METHODS get_tadir_info
      RETURNING
        VALUE(rs_tadir_info) TYPE zif_sat_ty_global=>ty_s_cds_tadir.

    "! <p class="shorttext synchronized">Returns base tables for CDS View</p>
    METHODS get_base_tables
      RETURNING
        VALUE(result) TYPE zsat_cds_view_base_table_t.

    "! <p class="shorttext synchronized">Returns columns information</p>
    METHODS get_columns
      IMPORTING
        if_key_only   TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE dd03ndvtab.

    "! <p class="shorttext synchronized">Retrieve description of cds view</p>
    "!
    "! @parameter rv_description | <p class="shorttext synchronized"></p>
    METHODS get_description
      RETURNING
        VALUE(rv_description) TYPE ddtext.

    "! <p class="shorttext synchronized">Returns header information</p>
    METHODS get_header
      RETURNING
        VALUE(result) TYPE zsat_cds_view_header.

    "! <p class="shorttext synchronized">Returns parameter information</p>
    METHODS get_parameters
      IMPORTING
        if_exclude_system_params TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)            TYPE zif_sat_ty_global=>ty_t_cds_parameter.

    "! <p class="shorttext synchronized">Returns 'X' if cds view has associations</p>
    METHODS has_associations
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Checks if this cds view has parameters</p>
    METHODS has_parameters
      IMPORTING
        if_exclude_system_params TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)            TYPE abap_bool.

    "! <p class="shorttext synchronized">Returns 'X' if this CDS is of type Analytics.query = true</p>
    METHODS is_analytics_query
      RETURNING
        VALUE(rf_is_query) TYPE abap_bool.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Header for CDS View</p>
    DATA ms_header TYPE zsat_cds_view_header.
    "! <p class="shorttext synchronized">Parameters for cds view</p>
    DATA mt_parameters TYPE zif_sat_ty_global=>ty_t_cds_parameter.
    "! <p class="shorttext synchronized">DD: Table for Node Attributes</p>
    DATA mt_columns TYPE dd03ndvtab.
    DATA mt_api_states TYPE zif_sat_ty_global=>ty_t_cds_api_state.
    DATA mt_association_header TYPE dd08bvtab.
    DATA mt_association_field TYPE dd05bvtab.
    DATA mf_has_associations TYPE abap_bool.
    "! <p class="shorttext synchronized">Associations for CDS View</p>
    DATA mt_association TYPE zif_sat_ty_global=>ty_t_cds_association.
    "! <p class="shorttext synchronized">List Base tables of CDS View</p>
    DATA mt_base_tables TYPE zsat_cds_view_base_table_t.
    DATA mf_base_tables_loaded TYPE abap_bool.
    DATA mf_api_states_loaded TYPE abap_bool.
    DATA mf_tadir_info_loaded TYPE abap_bool.
    DATA ms_tadir_info TYPE zif_sat_ty_global=>ty_s_cds_tadir.
    DATA mt_annotations TYPE zif_sat_ty_global=>ty_t_cds_annotation.
    DATA mv_description TYPE ddtext.
    DATA mf_description_loaded TYPE abap_bool.
ENDCLASS.


CLASS zcl_sat_cds_view IMPLEMENTATION.
  METHOD constructor.
    ms_header = is_header.
    mv_view_name = is_header-entityname.
    mt_parameters  = it_parameters.
    mt_columns     = it_columns.
    mt_association = it_association.
    IF it_base_tables IS SUPPLIED.
      mf_base_tables_loaded = abap_true.
      mt_base_tables        = it_base_tables.
    ENDIF.
    IF it_api_states IS SUPPLIED.
      mf_api_states_loaded = abap_true.
      mt_api_states = it_api_states.
    ENDIF.
    mf_has_associations = xsdbool( mt_association IS NOT INITIAL ).

    mv_association_count = lines( it_association ).
    mv_select_table_count = lines( it_base_tables ).
  ENDMETHOD.

  METHOD get_annotations.
    " Check if the requested annotations were already read
    LOOP AT mt_annotations ASSIGNING FIELD-SYMBOL(<ls_annotation>) WHERE name IN it_annotation_name.
    ENDLOOP.

    IF sy-subrc <> 0.
      RAISE EVENT request_annotations
        EXPORTING et_anno_name_range = it_annotation_name.
    ENDIF.

    LOOP AT mt_annotations ASSIGNING <ls_annotation> WHERE name IN it_annotation_name.
      rt_annotation = VALUE #( BASE rt_annotation ( <ls_annotation> ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_api_states.
    IF mf_api_states_loaded = abap_false.
      RAISE EVENT request_api_states.
      mf_api_states_loaded = abap_true.
    ENDIF.

    rt_api_states = mt_api_states.
  ENDMETHOD.

  METHOD get_associations.
    result = mt_association.
  ENDMETHOD.

  METHOD get_base_tables.
    " were the base tables already loaded?
    IF mf_base_tables_loaded = abap_false.
      RAISE EVENT request_base_tables.
      mf_base_tables_loaded = abap_true.
    ENDIF.
    result = mt_base_tables.
  ENDMETHOD.

  METHOD get_columns.
    IF if_key_only = abap_true.
      result = VALUE #( FOR key IN mt_columns WHERE ( keyflag = abap_true ) ( key ) ).
    ELSE.
      result = mt_columns.
    ENDIF.
  ENDMETHOD.

  METHOD get_description.
    IF mf_description_loaded = abap_false.
      RAISE EVENT request_description.
      mf_description_loaded = abap_true.
    ENDIF.

    rv_description = mv_description.
  ENDMETHOD.

  METHOD get_header.
    result = ms_header.
  ENDMETHOD.

  METHOD get_parameters.
    result = mt_parameters.

    IF if_exclude_system_params = abap_true.
      DELETE result WHERE has_system_anno = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_tadir_info.
    IF mf_tadir_info_loaded = abap_false.
      RAISE EVENT request_tadir_info.
      mf_tadir_info_loaded = abap_true.
    ENDIF.

    rs_tadir_info = ms_tadir_info.
  ENDMETHOD.

  METHOD has_associations.
    result = mf_has_associations.
  ENDMETHOD.

  METHOD has_parameters.
    IF if_exclude_system_params = abap_true.
      result = xsdbool( line_exists( mt_parameters[ has_system_anno = abap_false ] ) ).
    ELSE.
      result = xsdbool( mt_parameters IS NOT INITIAL ).
    ENDIF.
  ENDMETHOD.

  METHOD is_analytics_query.
    DATA(lt_anno) = get_annotations( VALUE #( ( sign = 'I' option = 'EQ' low = 'ANALYTICS.QUERY' ) ) ).
    IF lt_anno IS NOT INITIAL.
      DATA(lv_query_anno_val) = VALUE #( lt_anno[ 1 ]-value DEFAULT 'FALSE' ).
      rf_is_query = xsdbool( lv_query_anno_val = 'TRUE' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
