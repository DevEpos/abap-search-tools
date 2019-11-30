CLASS zcl_sat_adt_res_object_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get
        REDEFINITION.
    METHODS constructor
      IMPORTING
        iv_search_type TYPE zif_sat_ty_object_search=>ty_search_type.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_package,
        package      TYPE devclass,
        createdby    TYPE username,
        superpackage TYPE devclass,
        description  TYPE ddtext,
      END OF ty_s_package,
      BEGIN OF ty_s_ddls_source,
        ddlname TYPE ddlname,
        source  TYPE string,
      END OF ty_s_ddls_source,
      ty_lt_ddlname TYPE RANGE OF ddlname.

    DATA mt_query_result TYPE zsat_entity_t.
    DATA mf_with_package_hierarchy TYPE abap_bool.
    DATA mv_search_type TYPE zif_sat_ty_object_search=>ty_search_type.
    DATA mt_super_package_all TYPE SORTED TABLE OF ty_s_package WITH UNIQUE KEY package.
    DATA mt_ddls_source       TYPE HASHED TABLE OF ty_s_ddls_source WITH UNIQUE KEY ddlname.
    DATA mo_search_config TYPE REF TO zif_sat_object_search_config.

    "! <p class="shorttext synchronized" lang="en">Retrieve query parameters from request</p>
    METHODS get_query_parameter
      IMPORTING
        io_request              TYPE REF TO if_adt_rest_request
      EXPORTING
        ev_query                TYPE string
        es_search_engine_params TYPE zif_sat_ty_object_search=>ty_s_search_engine_params
        et_options              TYPE zif_sat_ty_object_search=>ty_t_search_option
      RAISING
        cx_adt_rest .

    "! <p class="shorttext synchronized" lang="en">Retrieve values of request parameter</p>
    METHODS get_request_param_values
      IMPORTING
        iv_param_name    TYPE string
        io_request       TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rt_values) TYPE string_table.
    "! <p class="shorttext synchronized" lang="en">Get search parameter value(s) from request</p>
    "! @parameter iv_option_name | Optional name for search option<br>
    "!      If not supplied the option name will be the parameter name
    METHODS get_search_parameter
      IMPORTING
        io_request      TYPE REF TO if_adt_rest_request
        iv_param_name   TYPE string
        iv_option_name  TYPE string OPTIONAL
        if_single_value TYPE abap_bool OPTIONAL
      CHANGING
        ct_option       TYPE zif_sat_ty_object_search=>ty_t_search_option.
    "! <p class="shorttext synchronized" lang="en">Retrieve value of request parameter</p>
    METHODS get_request_param_value
      IMPORTING
        iv_param_name   TYPE string
        io_request      TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rv_value) TYPE string.

    "! <p class="shorttext synchronized" lang="en">Post processing for a search result entry</p>
    METHODS post_process_result_entry
      IMPORTING
        is_result_entity TYPE zsat_entity
      CHANGING
        cs_result        TYPE zsat_adt_element_info.
    "! <p class="shorttext synchronized" lang="en">Sets the positional URI for the given DDLS result</p>
    METHODS set_ddl_positional_uri
      IMPORTING
        is_result_entity TYPE zsat_entity
      CHANGING
        cs_result        TYPE zsat_adt_element_info.
    "! <p class="shorttext synchronized" lang="en">Post processing for search results</p>
    METHODS post_process_result.
    "! <p class="shorttext synchronized" lang="en">Reads DDL Sources in for search result</p>
    "!
    METHODS read_ddl_sources.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Create response</p>
    METHODS create_response
      RETURNING
        VALUE(result) TYPE zsat_adt_element_info_t.
    "! <p class="shorttext synchronized" lang="en">Determine the package hierarchy of the search result</p>
    METHODS determine_package_hierarchy.
ENDCLASS.



CLASS zcl_sat_adt_res_object_search IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_search_type = iv_search_type.
    mo_search_config = CAST #( zcl_sat_ioc_lookup=>get_instance(
                                 iv_contract = 'zif_sat_object_search_config'
                                 iv_filter   = |{ mv_search_type }| ) ).
  ENDMETHOD.

  METHOD get.
    CHECK: mv_search_type IS NOT INITIAL,
           mo_search_config IS BOUND.

    get_query_parameter(
      EXPORTING
        io_request              = request
      IMPORTING
        ev_query                = DATA(lv_query)
        es_search_engine_params = DATA(ls_search_engine_params)
        et_options              = DATA(lt_options)
    ).

*.. Determine if the package hierarchy of the result should also be determined
    mf_with_package_hierarchy = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_object_search=>c_general_search_params-read_package_hierarchy
        iv_default_value = abap_false
        io_request       = request ).

    TRY.

        DATA(lo_search_engine) = CAST zif_sat_search_engine( zcl_sat_ioc_lookup=>get_instance(
                                      iv_contract = 'zif_sat_search_engine' ) ).
        lo_search_engine->search_objects(
          EXPORTING iv_search_terms         = lv_query
                    it_options              = lt_options
                    iv_search_type          = mv_search_type
                    is_search_engine_params = ls_search_engine_params
          IMPORTING et_results              = mt_query_result
        ).

      CATCH zcx_sat_object_search INTO DATA(lx_search_error).
        RAISE EXCEPTION TYPE zcx_sat_adt_object_search
          EXPORTING
            textid = cx_adt_rest=>create_textid_from_exc_text( exception = lx_search_error ).
    ENDTRY.

    response->set_body_data(
        content_handler = zcl_sat_adt_ch_factory=>create_gen_eleminfos_res_ch( )
        data            = create_response( )
    ).
  ENDMETHOD.

  METHOD read_ddl_sources.

    DATA lt_ddlname TYPE ty_lt_ddlname.

    lt_ddlname = VALUE #( FOR res IN mt_query_result
                          WHERE ( tadir_type  = 'DDLS' )
                          ( sign = 'I' option = 'EQ' low = res-secondary_entity_id ) ).

    SELECT ddlname,
           source
       FROM ddddlsrc
       WHERE as4local = 'A'
         AND ddlname  IN @lt_ddlname
    INTO CORRESPONDING FIELDS OF TABLE @mt_ddls_source.

  ENDMETHOD.

  METHOD get_query_parameter.
    ev_query = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_object_search=>c_general_search_params-object_name
        io_request       = io_request
    ).
    es_search_engine_params = VALUE #(
      use_and_cond_for_options = zcl_sat_adt_res_util=>get_request_param_value(
        iv_param_name    = zif_sat_c_object_search=>c_general_search_params-use_and_for_filters
        iv_default_value = abap_false
        io_request       = io_request )
      with_api_state = zcl_sat_adt_res_util=>get_request_param_value(
         iv_param_name         = zif_sat_c_object_search=>c_general_search_params-read_api_state
         iv_default_value = abap_false
         io_request       = io_request )
      get_all                  = zcl_sat_adt_res_util=>get_request_param_value(
         iv_param_name    = zif_sat_c_object_search=>c_general_search_params-get_all_results
         iv_default_value = abap_false
         io_request       = io_request )
    ).

    LOOP AT mo_search_config->get_options( ) ASSIGNING FIELD-SYMBOL(<ls_option>).
      get_search_parameter(
        EXPORTING io_request      = io_request
                  iv_param_name   = <ls_option>-option
                  if_single_value = <ls_option>-single
        CHANGING  ct_option       = et_options
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_response.

    IF mt_query_result IS NOT INITIAL.
      post_process_result( ).
    ENDIF.

    APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<ls_result>).
    <ls_result>-type = 'rawResult'.
    DATA(lr_t_raw_result) = NEW zsat_adt_element_info_t( ).
    <ls_result>-children = lr_t_raw_result.
    ASSIGN lr_t_raw_result->* TO FIELD-SYMBOL(<lt_raw_result>).

    LOOP AT mt_query_result ASSIGNING FIELD-SYMBOL(<ls_search_result>).
      APPEND INITIAL LINE TO <lt_raw_result> ASSIGNING <ls_result>.
      DATA(ls_object_reference) = zcl_sat_adt_util=>create_adt_uri(
            iv_type       = <ls_search_result>-entity_type
            iv_tadir_type = <ls_search_result>-tadir_type
            iv_name       = <ls_search_result>-entity_id
            iv_name2      = <ls_search_result>-secondary_entity_id ).
      <ls_result> = VALUE #(
        name             = <ls_search_result>-entity_id
        raw_name         = <ls_search_result>-entity_id_raw
        package          = <ls_search_result>-devclass
        entity_type      = <ls_search_result>-entity_type
        type             = ls_object_reference-type
        uri              = ls_object_reference-uri
        description      = <ls_search_result>-description
        owner            = <ls_search_result>-created_by
        properties       = VALUE #(
          ( key = 'API_STATE'   value = <ls_search_result>-api_state )
          ( key = 'SOURCE_TYPE' value = <ls_search_result>-source_type )
        )
      ).
      post_process_result_entry(
        EXPORTING is_result_entity = <ls_search_result>
        CHANGING  cs_result        = <ls_result>
      ).
    ENDLOOP.

*.. Add package hierarchy entries if it was requested
    IF mf_with_package_hierarchy = abap_true.
      determine_package_hierarchy( ).

      CHECK mt_super_package_all IS NOT INITIAL.

      APPEND INITIAL LINE TO result ASSIGNING <ls_result>.
      <ls_result>-type = 'packages'.
      DATA(lr_packages) = NEW zsat_adt_element_info_t( ).
      <ls_result>-children = lr_packages.
      ASSIGN lr_packages->* TO FIELD-SYMBOL(<lt_package>).

      LOOP AT mt_super_package_all ASSIGNING FIELD-SYMBOL(<ls_package>).
        APPEND INITIAL LINE TO <lt_package> ASSIGNING <ls_result>.
        ls_object_reference = zcl_sat_adt_util=>create_adt_uri(
              iv_tadir_type = 'DEVC'
              iv_name       = <ls_package>-package ).
        <ls_result> = VALUE #(
          name        = <ls_package>-package
          raw_name    = <ls_package>-package
          description = <ls_package>-description
          package     = <ls_package>-superpackage
          owner       = <ls_package>-createdby
          type        = ls_object_reference-type
          uri         = ls_object_reference-uri
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD determine_package_hierarchy.
    TYPES: BEGIN OF lty_package,
             devclass TYPE devclass,
           END OF lty_package.
    TYPES: BEGIN OF lty_pack_description,
             developementpackage TYPE devclass,
             createdby           TYPE user,
             description         TYPE ddtext,
           END OF lty_pack_description.

    DATA: lt_package      TYPE STANDARD TABLE OF lty_package,
          lt_package_info TYPE SORTED TABLE OF lty_pack_description WITH UNIQUE KEY developementpackage.

    lt_package = CORRESPONDING #( mt_query_result ).
    SORT lt_package.
    DELETE ADJACENT DUPLICATES FROM lt_package.

    LOOP AT lt_package INTO DATA(ls_package).
      cl_pak_package_queries=>get_superpackages(
        EXPORTING
          im_package                     = ls_package-devclass
        IMPORTING
          et_superpackages               = DATA(lt_super_package)
        EXCEPTIONS
          OTHERS                         = 1
      ).
      LOOP AT lt_super_package ASSIGNING FIELD-SYMBOL(<ls_super_package>).
        INSERT CORRESPONDING #( <ls_super_package> ) INTO TABLE mt_super_package_all.
      ENDLOOP.
    ENDLOOP.

*.. Determine package descriptions
    IF mt_super_package_all IS NOT INITIAL.
      SELECT developmentpackage,
             createdby,
             description
         FROM zsat_i_developmentpackage( p_language = @sy-langu )
         FOR ALL ENTRIES IN @mt_super_package_all
         WHERE developmentpackage = @mt_super_package_all-package
      INTO TABLE @lt_package_info.

      LOOP AT lt_package_info ASSIGNING FIELD-SYMBOL(<ls_package_info>).
        ASSIGN mt_super_package_all[ package = <ls_package_info>-developementpackage ] TO FIELD-SYMBOL(<ls_package>).
        IF sy-subrc = 0.
          <ls_package>-description = <ls_package_info>-description.
          <ls_package>-createdby = <ls_package_info>-createdby.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_request_param_value.
    TRY.
        io_request->get_uri_query_parameter(
          EXPORTING name    = iv_param_name
          IMPORTING value   = rv_value
        ).
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.

  METHOD get_request_param_values.
    TRY.
        io_request->get_uri_query_parameter_values(
          EXPORTING name    = iv_param_name
          IMPORTING values  = rt_values
        ).
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.

  METHOD get_search_parameter.
    DATA: lt_option_values TYPE zif_sat_ty_object_search=>ty_t_search_option.

    DATA(lv_option_name) = COND #( WHEN iv_option_name IS NOT INITIAL THEN iv_option_name ELSE iv_param_name ).
    IF if_single_value = abap_true.
      DATA(lv_param_value) = get_request_param_value(
        iv_param_name = iv_param_name
        io_request    = io_request
      ).
      IF lv_param_value IS NOT INITIAL.
        ct_option = VALUE #( BASE ct_option ( option      = lv_option_name
                                              value_range = VALUE #( ( low = lv_param_value ) ) ) ).
      ENDIF.
    ELSE.
      DATA(lt_param_values) = get_request_param_values(
        iv_param_name = iv_param_name
        io_request    = io_request
      ).
      IF lt_param_values IS NOT INITIAL.
        ct_option = VALUE #( BASE ct_option ( option      = lv_option_name
                                              value_range = VALUE #( FOR value IN lt_param_values ( low = value ) ) ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD post_process_result_entry ##needed.
  ENDMETHOD.


  METHOD post_process_result ##needed.
  ENDMETHOD.

  METHOD set_ddl_positional_uri.
    DATA: lt_ddls_source_lines TYPE TABLE OF string.

    ASSIGN mt_ddls_source[ ddlname = is_result_entity-secondary_entity_id ] TO FIELD-SYMBOL(<ls_source>).
    CHECK sy-subrc = 0.

    zcl_sat_cds_view_factory=>get_entityname_pos_in_ddlsrc(
      EXPORTING iv_entity_id = is_result_entity-entity_id
                iv_source    = <ls_source>-source
      IMPORTING ev_column    = DATA(lv_col)
                ev_row       = DATA(lv_row)
    ).
    IF lv_col <> -1 AND lv_row <> -1.
*...... Adjust ADT URI
      cs_result-uri = |{ cs_result-uri }{ zif_sat_c_adt_utils=>c_ddl_pos_uri_segment }{ lv_row },{ lv_col }|.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
