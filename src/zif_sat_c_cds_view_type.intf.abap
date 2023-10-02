INTERFACE zif_sat_c_cds_view_type
  PUBLIC.

  CONSTANTS view TYPE zsat_cds_source_type VALUE 'V' ##NO_TEXT.
  CONSTANTS extend TYPE zsat_cds_source_type VALUE 'E' ##NO_TEXT.
  CONSTANTS table_function TYPE zsat_cds_source_type VALUE 'F' ##NO_TEXT.
  CONSTANTS table_entity TYPE zsat_cds_source_type VALUE 'T' ##NO_TEXT.
  CONSTANTS abstract_entity TYPE zsat_cds_source_type VALUE 'A' ##NO_TEXT.
  CONSTANTS custom_entity TYPE zsat_cds_source_type VALUE 'Q' ##NO_TEXT.
  CONSTANTS hierarchy TYPE zsat_cds_source_type VALUE 'H' ##NO_TEXT.
  CONSTANTS projection TYPE zsat_cds_source_type VALUE 'P' ##NO_TEXT.
ENDINTERFACE.
