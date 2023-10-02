INTERFACE zif_sat_c_entity_type
  PUBLIC.

  CONSTANTS table TYPE zsat_entity_type VALUE 'T' ##NO_TEXT.
  CONSTANTS view TYPE zsat_entity_type VALUE 'V' ##NO_TEXT.
  CONSTANTS cds_view TYPE zsat_entity_type VALUE 'C' ##NO_TEXT.
  CONSTANTS query TYPE zsat_entity_type VALUE 'Q' ##NO_TEXT.
ENDINTERFACE.
