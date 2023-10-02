INTERFACE zif_sat_c_join_cond_val_type
  PUBLIC.

  CONSTANTS system_value_input TYPE zsat_join_cond_value_type VALUE 'SYSTEM' ##NO_TEXT.
  CONSTANTS typed_input TYPE zsat_join_cond_value_type VALUE 'TYPED' ##NO_TEXT.
  CONSTANTS parameter_input TYPE zsat_join_cond_value_type VALUE 'PARAM' ##NO_TEXT.
ENDINTERFACE.
