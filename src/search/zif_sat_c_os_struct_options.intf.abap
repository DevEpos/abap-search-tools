"! <p class="shorttext synchronized">General options for Structure object search</p>
INTERFACE zif_sat_c_os_struct_options
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_structure_type,
      structure        TYPE zsat_i_structure-type VALUE 'STRUCT',
      append_structure TYPE zsat_i_structure-type VALUE 'APPEND_STRUCT',
    END OF c_structure_type.
ENDINTERFACE.
