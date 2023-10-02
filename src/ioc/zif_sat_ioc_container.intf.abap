"! <p class="shorttext synchronized">IoC Container</p>
"! An IoC container implementation represents a more complex version
"! of dependency injection (<strong>DI</strong>) than <em>GET_INSTANCE</em> in {@link ZCL_SAT_INSTANCE_LOOKUP}
"! can offer. <br>
"! In an IoC you can define contracts that have dependencies in their CONSTRUCTORs
INTERFACE zif_sat_ioc_container
  PUBLIC.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Table type of IoC Containers</p>
    ty_t_ioc TYPE STANDARD TABLE OF REF TO zif_sat_ioc_container,

    "! <p class="shorttext synchronized" lang="en">Contract definition</p>
    BEGIN OF ty_s_contract,
      name TYPE classname,
      ref  TYPE REF TO zif_sat_ioc_contract,
    END OF ty_s_contract,
    "! <p class="shorttext synchronized" lang="en">List of contract definitions</p>
    ty_t_contract TYPE SORTED TABLE OF ty_s_contract WITH UNIQUE KEY name.

  "! <p class="shorttext synchronized">Retrieves implementer for the given contract</p>
  "!
  "! @parameter iv_contract | <p class="shorttext synchronized">The name of the contract interface</p>
  "! @parameter iv_filter   | <p class="shorttext synchronized">Optional filter value to get specific implementer</p>
  "! @parameter ro_obj      | <p class="shorttext synchronized">The implementing instance for the contract</p>
  METHODS get_implementer
    IMPORTING
      iv_contract   TYPE classname
      iv_filter     TYPE string OPTIONAL
    RETURNING
      VALUE(ro_obj) TYPE REF TO object.

  "! <p class="shorttext synchronized">Cleans up cached instances of this IoC</p>
  "!
  METHODS clean_up.

ENDINTERFACE.
