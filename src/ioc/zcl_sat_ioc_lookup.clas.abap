"! <p class="shorttext synchronized">Instance Lookup via IoC</p>
CLASS zcl_sat_ioc_lookup DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_t_object_ref TYPE STANDARD TABLE OF REF TO object WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Returns an instance to the particular interface</p>
    "!
    "! @parameter iv_contract | <p class="shorttext synchronized">ABAP OO-Interface Name</p>
    "! @parameter iv_filter   | <p class="shorttext synchronized">Optional filter to get specific implementer</p>
    "! @parameter ro_obj      | <p class="shorttext synchronized">Created object</p>
    CLASS-METHODS get_instance
      IMPORTING
        iv_contract   TYPE classname
        iv_filter     TYPE string OPTIONAL
      RETURNING
        VALUE(ro_obj) TYPE REF TO object.

    "! <p class="shorttext synchronized">Cleans up the internal instances cache</p>
    CLASS-METHODS cleanup.

    "! <p class="shorttext synchronized">Injects a list of IoC container references</p>
    "! Calling this method will override the global available list of IoC containers <br/>
    "! @parameter it_ioc | <p class="shorttext synchronized">Table of IoC container instances</p>
    CLASS-METHODS inject_ioc_containers
      IMPORTING
        it_ioc TYPE zif_sat_ioc_container=>ty_t_ioc.

    "! <p class="shorttext synchronized">Resolves the list of dependencies</p>
    "!
    "! @parameter it_dependency          | <p class="shorttext synchronized">List of constructor dependencies</p>
    "! @parameter et_resolved_dependency | <p class="shorttext synchronized">Resolved constructor dependencies</p>
    CLASS-METHODS resolve_dependencies
      IMPORTING
        it_dependency          TYPE zif_sat_ioc_contract=>ty_t_dependencies
      EXPORTING
        et_resolved_dependency TYPE zif_sat_ioc_contract=>ty_t_resolved_dependency.

  PRIVATE SECTION.
    CONSTANTS c_ioc_container_intf_name TYPE string VALUE 'ZCL_SAT_BASE_IOC' ##NO_TEXT.

    CLASS-DATA gt_ioc_containers TYPE zif_sat_ioc_container=>ty_t_ioc.

    "! <p class="shorttext synchronized">Retrieves implementer for contract</p>
    "!
    "! @parameter iv_contract    | <p class="shorttext synchronized">Requested interface contract</p>
    "! @parameter iv_filter      | <p class="shorttext synchronized">Optional filter value</p>
    "! @parameter ro_implementer | <p class="shorttext synchronized">The found/created implementer</p>
    CLASS-METHODS get_implementer
      IMPORTING
        iv_contract           TYPE classname
        iv_filter             TYPE string
      RETURNING
        VALUE(ro_implementer) TYPE REF TO object.

    "! <p class="shorttext synchronized">Loads a list of IoC containers</p>
    "! All containers that implement the interface {@link ZIF_SAT_IOC_CONTAINER} will
    "! be loaded.
    CLASS-METHODS load_ioc_containers.
ENDCLASS.


CLASS zcl_sat_ioc_lookup IMPLEMENTATION.
  METHOD cleanup.
    FREE gt_ioc_containers.
  ENDMETHOD.

  METHOD get_instance.
    ro_obj = get_implementer( iv_contract = to_upper( iv_contract )
                              iv_filter   = iv_filter ).
    ASSERT ro_obj IS BOUND.
  ENDMETHOD.

  METHOD inject_ioc_containers.
    gt_ioc_containers = it_ioc.
  ENDMETHOD.

  METHOD resolve_dependencies.
    DATA lr_contract_type TYPE REF TO data.

    LOOP AT it_dependency ASSIGNING FIELD-SYMBOL(<ls_dependency>).
      DATA(lo_dependency) = get_implementer( iv_contract = <ls_dependency>-contract
                                             iv_filter   = <ls_dependency>-filter ).
      ASSERT lo_dependency IS BOUND.
      CREATE DATA lr_contract_type TYPE REF TO (<ls_dependency>-contract).
      ASSIGN lr_contract_type->* TO FIELD-SYMBOL(<lo_contract_type>).
      <lo_contract_type> ?= lo_dependency.
      et_resolved_dependency = VALUE #( BASE et_resolved_dependency
                                        ( parameter = <ls_dependency>-parameter ref = lr_contract_type ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_implementer.
    IF gt_ioc_containers IS INITIAL.
      load_ioc_containers( ).
    ENDIF.

    " 1) check the register if this instance was already loaded
    IF gt_ioc_containers IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT gt_ioc_containers INTO DATA(lo_ioc_container).
      ro_implementer = lo_ioc_container->get_implementer( iv_contract = iv_contract
                                                          iv_filter   = iv_filter ).
      CHECK ro_implementer IS BOUND.
      RETURN.
    ENDLOOP.

    ASSERT ro_implementer IS BOUND.
  ENDMETHOD.

  METHOD load_ioc_containers.
    DATA lo_ioc_container TYPE REF TO zif_sat_ioc_container.

    SELECT clsname AS classname FROM seometarel
      WHERE refclsname = @c_ioc_container_intf_name
        AND version    = @seoc_version_active
        AND impabstrct = @abap_false
        AND state      = @seoc_state_implemented
        AND reltype    = '2' " Inheritance
      INTO TABLE @DATA(lt_ioc_container).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_ioc_container INTO DATA(ls_ioc_cont_impl).
      CLEAR lo_ioc_container.
      CREATE OBJECT lo_ioc_container TYPE (ls_ioc_cont_impl-classname).
      gt_ioc_containers = VALUE #( BASE gt_ioc_containers ( lo_ioc_container ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
