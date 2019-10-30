INTERFACE zif_sat_c_join_types
  PUBLIC .
  CONSTANTS:
    inner_join       TYPE zsat_jointype VALUE 'IN' ##no_text,
    left_outer_join  TYPE zsat_jointype VALUE 'LO' ##no_text,
    right_outer_join TYPE zsat_jointype VALUE 'RO' ##no_text.
ENDINTERFACE.
