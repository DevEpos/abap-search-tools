"! <p class="shorttext synchronized">Contract for IoC</p>
INTERFACE zif_sat_ioc_contract
  PUBLIC.
  TYPES:
    "! <p class="shorttext synchronized" lang="en">Lifetime of instances in IoC</p>
    ty_lifetime TYPE i,
    "! <p class="shorttext synchronized" lang="en">Dependency definition for constructor DI</p>
    BEGIN OF ty_s_dependency,
      parameter TYPE seocmpname,
      contract  TYPE classname,
      filter    TYPE string,
    END OF ty_s_dependency,
    "! <p class="shorttext synchronized" lang="en">List of Constructor DI definitions</p>
    ty_t_dependencies TYPE STANDARD TABLE OF ty_s_dependency WITH KEY parameter,

    "! <p class="shorttext synchronized" lang="en">Describes an implementer of an interface</p>
    BEGIN OF ty_s_implementer,
      filter       TYPE string,
      implementer  TYPE classname,
      instance     TYPE REF TO object,
      is_injected  TYPE abap_bool,
      dependencies TYPE ty_t_dependencies,
    END OF ty_s_implementer,
    "! <p class="shorttext synchronized" lang="en">Table type of implementers</p>
    ty_t_implementer TYPE SORTED TABLE OF ty_s_implementer WITH UNIQUE KEY filter,
    "! <p class="shorttext synchronized" lang="en">Maps filter of a contract to another</p>
    BEGIN OF ty_s_filter_map,
      filter        TYPE string,
      mapped_filter TYPE string,
    END OF ty_s_filter_map,
    "! <p class="shorttext synchronized" lang="en">A table of filter mappings for implementers</p>
    ty_t_filter_map TYPE HASHED TABLE OF ty_s_filter_map WITH UNIQUE KEY filter,

    "! <p class="shorttext synchronized" lang="en">Resolved constructor dependency</p>
    BEGIN OF ty_s_resolved_dependency,
      parameter TYPE seocmpname,
      ref       TYPE REF TO data,
    END OF ty_s_resolved_dependency,
    "! <p class="shorttext synchronized" lang="en">List of resolved constructor dependencies</p>
    ty_t_resolved_dependency TYPE STANDARD TABLE OF ty_s_resolved_dependency WITH KEY parameter.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Lifetime for instance in IoC container</p>
    BEGIN OF c_lifetime,
      "! <p class="shorttext synchronized" lang="en">Instance is bound to IoC lifetime</p>
      ioc       TYPE ty_lifetime VALUE 0,
      "! <p class="shorttext synchronized" lang="en">Each request creates a new instance</p>
      transient TYPE ty_lifetime VALUE 1,
    END OF c_lifetime.

  "! <p class="shorttext synchronized">Name of the contract</p>
  DATA mv_name TYPE classname READ-ONLY.

  "! <p class="shorttext synchronized">Maps the filter value to another</p>
  "!
  "! @parameter iv_filter        | <p class="shorttext synchronized">The original filter value</p>
  "! @parameter iv_mapped_filter | <p class="shorttext synchronized">The mapped filter value</p>
  METHODS map_filter
    IMPORTING
      iv_filter        TYPE string
      iv_mapped_filter TYPE string.

  "! <p class="shorttext synchronized">Adds new implementer to contract</p>
  "!
  "! @parameter iv_filter       | <p class="shorttext synchronized">Optional filter value</p>
  "! @parameter iv_implementer  | <p class="shorttext synchronized">The name of the implementing class</p>
  "! @parameter it_dependencies | <p class="shorttext synchronized">Optional list of Constructor DIs</p>
  "! @parameter ro_contract     | <p class="shorttext synchronized">Returns self</p>
  METHODS add_implementer
    IMPORTING
      iv_filter          TYPE string            OPTIONAL
      iv_implementer     TYPE classname
      it_dependencies    TYPE ty_t_dependencies OPTIONAL
    RETURNING
      VALUE(ro_contract) TYPE REF TO zif_sat_ioc_contract.

  "! <p class="shorttext synchronized">Injects implementer instance</p>
  "! This method is primarily intended for Unit testing
  "!
  "! @parameter iv_filter   | <p class="shorttext synchronized">Optional filter value</p>
  "! @parameter io_instance | <p class="shorttext synchronized"></p>
  "! @parameter ro_contract | <p class="shorttext synchronized"></p>
  METHODS inject_implementer
    IMPORTING
      iv_filter          TYPE string OPTIONAL
      io_instance        TYPE REF TO object
    RETURNING
      VALUE(ro_contract) TYPE REF TO zif_sat_ioc_contract.

  "! <p class="shorttext synchronized">Retrieves implementer</p>
  "! An optional filter can be supplied
  "! @parameter iv_filter | <p class="shorttext synchronized">Optional filter value</p>
  "! @parameter ro_obj    | <p class="shorttext synchronized">The implementing class instance</p>
  METHODS get_implementer
    IMPORTING
      iv_filter     TYPE string OPTIONAL
    RETURNING
      VALUE(ro_obj) TYPE REF TO object.

  "! <p class="shorttext synchronized">Creates instance of implementer</p>
  "!
  "! @parameter iv_implementer           | <p class="shorttext synchronized">Name of implementing class</p>
  "! @parameter it_resolved_dependencies | <p class="shorttext synchronized">List of resolved constructor dependencies</p>
  "! @parameter ro_obj                   | <p class="shorttext synchronized">The created implementer instance</p>
  METHODS create_implementer
    IMPORTING
      iv_implementer           TYPE classname
      it_resolved_dependencies TYPE ty_t_resolved_dependency OPTIONAL
    RETURNING
      VALUE(ro_obj)            TYPE REF TO object.

  "! <p class="shorttext synchronized">Cleans up this contract implementers</p>
  "!
  METHODS clean_up.
ENDINTERFACE.
