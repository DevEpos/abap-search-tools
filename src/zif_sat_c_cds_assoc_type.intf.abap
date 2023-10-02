INTERFACE zif_sat_c_cds_assoc_type
  PUBLIC.

  CONSTANTS abstract_entity TYPE ddtargetkind VALUE 'A' ##NO_TEXT.
  CONSTANTS custom_entity TYPE ddtargetkind VALUE 'C' ##NO_TEXT.
  CONSTANTS entity TYPE ddtargetkind VALUE 'B' ##NO_TEXT.
  CONSTANTS view_entity TYPE ddtargetkind VALUE 'W' ##NO_TEXT.
  CONSTANTS view TYPE ddtargetkind VALUE 'J' ##NO_TEXT.
  CONSTANTS table TYPE ddtargetkind VALUE 'T' ##NO_TEXT.
  CONSTANTS table_function TYPE ddtargetkind VALUE 'F' ##NO_TEXT.
  CONSTANTS projection_entity TYPE ddtargetkind VALUE 'R' ##NO_TEXT.

ENDINTERFACE.
