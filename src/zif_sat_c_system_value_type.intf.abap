"! <p class="shorttext synchronized">Type of System Value (e.g. Date)</p>
INTERFACE zif_sat_c_system_value_type
  PUBLIC.
  CONSTANTS date TYPE zsat_syst_value_type VALUE 'DATE'.
  CONSTANTS time TYPE zsat_syst_value_type VALUE 'TIME'.
  CONSTANTS user TYPE zsat_syst_value_type VALUE 'USER'.
  CONSTANTS language TYPE zsat_syst_value_type VALUE 'LANGU'.
  CONSTANTS null TYPE zsat_syst_value_type VALUE 'NULL'.
  CONSTANTS not_null TYPE zsat_syst_value_type VALUE 'NOT NULL'.
ENDINTERFACE.
