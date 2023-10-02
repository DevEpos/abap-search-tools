INTERFACE zif_sat_c_join_types
  PUBLIC.
  CONSTANTS inner_join TYPE zsat_jointype VALUE 'IN' ##NO_TEXT.
  CONSTANTS left_outer_join TYPE zsat_jointype VALUE 'LO' ##NO_TEXT.
  CONSTANTS right_outer_join TYPE zsat_jointype VALUE 'RO' ##NO_TEXT.
ENDINTERFACE.
