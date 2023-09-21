"! <p class="shorttext synchronized">Search Engine to trigger Object Search</p>
CLASS zcl_sat_search_engine DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_search_engine.

  PRIVATE SECTION.
    ALIASES create_query            FOR zif_sat_search_engine~create_query.
    ALIASES search_objects_by_query FOR zif_sat_search_engine~search_objects_by_query.
ENDCLASS.


CLASS zcl_sat_search_engine IMPLEMENTATION.
  METHOD zif_sat_search_engine~create_query.
    DATA(lo_parser) = CAST zif_sat_object_query_parser( zcl_sat_ioc_lookup=>get_instance(
                                                            iv_contract = 'zif_sat_object_query_parser'
                                                            iv_filter   = |{ iv_search_type }| ) ).
    ro_query = lo_parser->create_query( it_search_terms = it_search_terms
                                        it_options      = it_options ).
  ENDMETHOD.

  METHOD zif_sat_search_engine~parse_query.
    DATA(lo_parser) = CAST zif_sat_object_query_parser( zcl_sat_ioc_lookup=>get_instance(
                                                            iv_contract = 'zif_sat_object_query_parser'
                                                            iv_filter   = |{ iv_search_type }| ) ).
    ro_query = lo_parser->parse_query( iv_search_query = iv_search_query ).
  ENDMETHOD.

  METHOD zif_sat_search_engine~search_objects.
    DATA(lo_query) = create_query( iv_search_type  = iv_search_type
                                   it_search_terms = it_search_terms
                                   it_options      = it_options ).
    lo_query->set_max_rows( iv_max_rows ).
    search_objects_by_query( EXPORTING io_query                = lo_query
                                       is_search_engine_params = is_search_engine_params
                             IMPORTING et_results              = et_results ).
  ENDMETHOD.

  METHOD zif_sat_search_engine~search_objects_by_query.
    DATA(lo_search_provider) = CAST zif_sat_object_search_provider( zcl_sat_ioc_lookup=>get_instance(
                                                                        iv_contract = 'zif_sat_object_search_provider'
                                                                        iv_filter   = |{ io_query->mv_type }| ) ).
    lo_search_provider->search( EXPORTING io_query                = io_query
                                          is_search_engine_params = is_search_engine_params
                                IMPORTING et_result               = et_results ).
  ENDMETHOD.
ENDCLASS.
