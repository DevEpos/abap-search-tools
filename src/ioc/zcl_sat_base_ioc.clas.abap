"! <p class="shorttext synchronized" lang="en">Base IoC container implementation</p>
CLASS zcl_sat_base_ioc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sat_ioc_container.
  PROTECTED SECTION.
    DATA mt_contracts TYPE zif_sat_ioc_container=>ty_t_contract.

    "! <p class="shorttext synchronized" lang="en">Adds new contract definition to IoC</p>
    "!
    "! @parameter iv_contract | <p class="shorttext synchronized" lang="en">The name of the interface contract</p>
    "! @parameter iv_lifetime | <p class="shorttext synchronized" lang="en">The lifetime for the contract</p>
    "! @parameter ro_contract | <p class="shorttext synchronized" lang="en">The created contract instance</p>
    METHODS add_contract
      IMPORTING
        iv_contract        TYPE classname
        iv_lifetime        TYPE zif_sat_ioc_contract=>ty_lifetime OPTIONAL
      RETURNING
        VALUE(ro_contract) TYPE REF TO zif_sat_ioc_contract.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sat_base_ioc IMPLEMENTATION.
  METHOD zif_sat_ioc_container~clean_up.
    LOOP AT mt_contracts ASSIGNING FIELD-SYMBOL(<ls_contract>) WHERE ref IS BOUND.
      <ls_contract>-ref->clean_up( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_sat_ioc_container~get_implementer.
    ASSIGN mt_contracts[ name = iv_contract ] TO FIELD-SYMBOL(<ls_contract>).
    CHECK sy-subrc = 0.

    ro_obj = <ls_contract>-ref->get_implementer(
        iv_filter = iv_filter
    ).
  ENDMETHOD.

  METHOD add_contract.
    ASSIGN mt_contracts[ name = iv_contract ] TO FIELD-SYMBOL(<ls_contract>).
    IF sy-subrc = 0.
      ro_contract = <ls_contract>-ref.
      RETURN.
    ENDIF.

    ro_contract = NEW zcl_sat_ioc_contract(
      iv_contract      = iv_contract
      iv_lifetime      = iv_lifetime
    ).
    mt_contracts = VALUE #( BASE mt_contracts ( name = iv_contract ref = ro_contract ) ).
  ENDMETHOD.

ENDCLASS.
