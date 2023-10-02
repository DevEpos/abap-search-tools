"! <p class="shorttext synchronized">Contract for IoC</p>
CLASS zcl_sat_ioc_contract DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_ioc_contract.

    ALIASES mv_name FOR zif_sat_ioc_contract~mv_name.

    METHODS constructor
      IMPORTING
        iv_contract TYPE classname
        iv_lifetime TYPE zif_sat_ioc_contract=>ty_lifetime.

  PROTECTED SECTION.
    DATA mv_lifetime TYPE zif_sat_ioc_contract=>ty_lifetime.
    DATA mt_implementer TYPE zif_sat_ioc_contract=>ty_t_implementer.
    DATA mt_filter_map TYPE zif_sat_ioc_contract=>ty_t_filter_map.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sat_ioc_contract IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_contract.
    mv_lifetime = iv_lifetime.
  ENDMETHOD.

  METHOD zif_sat_ioc_contract~add_implementer.
    ro_contract = me.

    IF line_exists( mt_implementer[ filter = iv_filter ] ).
      RETURN.
    ENDIF.

    mt_implementer = VALUE #( BASE mt_implementer
                              ( filter = iv_filter implementer = iv_implementer dependencies = it_dependencies ) ).
  ENDMETHOD.

  METHOD zif_sat_ioc_contract~inject_implementer.
    ro_contract = me.

    IF line_exists( mt_implementer[ filter = iv_filter ] ).
      RETURN.
    ENDIF.

    mt_implementer = VALUE #( BASE mt_implementer
                              ( filter = iv_filter instance = io_instance is_injected = abap_true ) ).
  ENDMETHOD.

  METHOD zif_sat_ioc_contract~get_implementer.
    ASSIGN mt_implementer[ filter = iv_filter ] TO FIELD-SYMBOL(<ls_implementer>).
    IF sy-subrc <> 0.
      DATA(lv_filter) = VALUE #( mt_filter_map[ filter = iv_filter ]-mapped_filter OPTIONAL ).
      ASSIGN mt_implementer[ filter = lv_filter ] TO <ls_implementer>.
    ENDIF.

    IF <ls_implementer> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    "  TODO: consider lifetime for object creation

    "  Was the instance already created?
    IF <ls_implementer>-instance IS BOUND.
      ro_obj = <ls_implementer>-instance.
      RETURN.
    ENDIF.

    IF <ls_implementer>-dependencies IS NOT INITIAL.
      zcl_sat_ioc_lookup=>resolve_dependencies( EXPORTING it_dependency          = <ls_implementer>-dependencies
                                                IMPORTING et_resolved_dependency = DATA(lt_resolved_dependencies) ).
    ENDIF.

    "  Finally create the requested object
    ro_obj = zif_sat_ioc_contract~create_implementer( iv_implementer           = <ls_implementer>-implementer
                                                      it_resolved_dependencies = lt_resolved_dependencies ).

    ASSERT ro_obj IS BOUND.

    IF mv_lifetime = zif_sat_ioc_contract=>c_lifetime-ioc.
      <ls_implementer>-instance = ro_obj.
    ENDIF.
  ENDMETHOD.

  METHOD zif_sat_ioc_contract~clean_up.
    LOOP AT mt_implementer ASSIGNING FIELD-SYMBOL(<ls_implementer>).
      CLEAR <ls_implementer>-instance.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_sat_ioc_contract~map_filter.
    CHECK NOT line_exists( mt_filter_map[ filter = iv_filter ] ).

    mt_filter_map = VALUE #( BASE mt_filter_map
                             ( filter = iv_filter mapped_filter = iv_mapped_filter ) ).
  ENDMETHOD.

  METHOD zif_sat_ioc_contract~create_implementer.
    DATA lt_param_tab TYPE abap_parmbind_tab.

    LOOP AT it_resolved_dependencies ASSIGNING FIELD-SYMBOL(<ls_dependency>).
      lt_param_tab = VALUE #( BASE lt_param_tab
                              ( name  = <ls_dependency>-parameter
                                kind  = cl_abap_objectdescr=>exporting
                                value = <ls_dependency>-ref ) ).
    ENDLOOP.

    TRY.
        IF lt_param_tab IS NOT INITIAL.
          CREATE OBJECT ro_obj TYPE (iv_implementer)
            PARAMETER-TABLE lt_param_tab.
        ELSE.
          CREATE OBJECT ro_obj TYPE (iv_implementer).
        ENDIF.
      CATCH cx_sy_create_object_error INTO DATA(lx_create_error). " TODO: variable is assigned but never used (ABAP cleaner)
*        MESSAGE exc_ref->get_text( ) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
