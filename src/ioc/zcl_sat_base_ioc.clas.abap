"! <p class="shorttext synchronized">Base IoC container implementation</p>
CLASS zcl_sat_base_ioc DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_ioc_container.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_implementer,
        filter       TYPE string,
        implementer  TYPE classname,
        dependencies TYPE zif_sat_ioc_contract=>ty_t_dependencies,
      END OF ty_s_implementer.
    TYPES ty_t_implementer TYPE SORTED TABLE OF ty_s_implementer WITH UNIQUE KEY filter.
    TYPES:
      BEGIN OF ty_contract_data,
        name         TYPE classname,
        implementers TYPE ty_t_implementer,
      END OF ty_contract_data.
    TYPES ty_t_contract_data TYPE STANDARD TABLE OF ty_contract_data WITH EMPTY KEY.

    DATA mt_contracts TYPE zif_sat_ioc_container=>ty_t_contract.

    "! Adds multiple contracts to this IoC container
    METHODS add_contracts
      IMPORTING
        it_contracts TYPE ty_t_contract_data.

    "! <p class="shorttext synchronized">Adds new contract definition to IoC</p>
    "!
    "! @parameter iv_contract | The name of the interface contract
    "! @parameter iv_lifetime | The lifetime for the contract
    "! @parameter ro_contract | The created contract instance
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
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ro_obj = <ls_contract>-ref->get_implementer( iv_filter = iv_filter ).
  ENDMETHOD.

  METHOD add_contract.
    ASSIGN mt_contracts[ name = iv_contract ] TO FIELD-SYMBOL(<ls_contract>).
    IF sy-subrc = 0.
      ro_contract = <ls_contract>-ref.
      RETURN.
    ENDIF.

    ro_contract = NEW zcl_sat_ioc_contract( iv_contract = iv_contract
                                            iv_lifetime = iv_lifetime ).
    mt_contracts = VALUE #( BASE mt_contracts ( name = iv_contract ref = ro_contract ) ).
  ENDMETHOD.

  METHOD add_contracts.
    LOOP AT it_contracts REFERENCE INTO DATA(lr_contract).
      DATA(lo_contract) = add_contract( lr_contract->name ).
      CHECK lo_contract IS BOUND.

      LOOP AT lr_contract->implementers REFERENCE INTO DATA(lr_implementer).
        lo_contract->add_implementer( iv_filter       = lr_implementer->filter
                                      iv_implementer  = lr_implementer->implementer
                                      it_dependencies = lr_implementer->dependencies ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
