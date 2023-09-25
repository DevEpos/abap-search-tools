"! <p class="shorttext synchronized">Constants for sources for sql select</p>
INTERFACE zif_sat_c_select_source_id
  PUBLIC.

  "! <p class="shorttext synchronized">Table for DDL Sources</p>
  CONSTANTS ddddlsrc TYPE string VALUE 'DDDDLSRC'.
  "! <p class="shorttext synchronized">Table for Texts of DDL Sources</p>
  CONSTANTS ddddlsrct TYPE string VALUE 'DDDDLSRCT'.
  "! <p class="shorttext synchronized">Table for Enduser texts of DDL Sources</p>
  CONSTANTS ddddlscr02bt TYPE string VALUE 'DDDDLSRC02BT'.
  "! <p class="shorttext synchronized">Table for dependent objects of database views</p>
  CONSTANTS ddldependency TYPE string VALUE 'DDLDEPENDENCY'.
  CONSTANTS zsat_i_ddldependency TYPE string VALUE 'ZSAT_I_DDLDEPENDENCY'.
  "! <p class="shorttext synchronized">Table for CDS Parameters</p>
  CONSTANTS dd10b TYPE string VALUE 'DD10B'.
  "! <p class="shorttext synchronized">Table for CDS Association Header</p>
  CONSTANTS dd08b TYPE string VALUE 'DD08B'.
  "! <p class="shorttext synchronized">Technical Settings of Table</p>
  CONSTANTS dd09l TYPE string VALUE 'DD09L'.
  "! <p class="shorttext synchronized">Base tables of dictionary views</p>
  CONSTANTS dd26s TYPE string VALUE 'DD26S'.
  CONSTANTS reposrc TYPE string VALUE 'REPOSRC'.  
  CONSTANTS zsat_i_messageclass TYPE string VALUE 'ZSAT_I_MESSAGECLASS'.
  CONSTANTS zsat_i_message TYPE string VALUE 'ZSAT_I_MESSAGE'.
  " <p class="shorttext synchronized">Table for Redefined Methods</p>
  CONSTANTS zsat_i_redefinedmethod TYPE string VALUE 'ZSAT_I_REDEFINEDMETHOD'.
  CONSTANTS zsat_i_associatedincds TYPE string VALUE 'ZSAT_I_ASSOCIATEDINCDS'.
  CONSTANTS zsat_i_cdsfrompartentity TYPE string VALUE 'ZSAT_I_CDSFROMPARTENTITY'.
  CONSTANTS zsat_i_ddicview TYPE string VALUE 'ZSAT_I_DDICVIEW'.
  CONSTANTS zsat_i_databasetable TYPE string VALUE 'ZSAT_I_DATABASETABLE'.
  CONSTANTS zsat_i_databaseentity TYPE string VALUE 'ZSAT_I_DATABASEENTITY'.
  CONSTANTS zsat_i_databasetablesandviews TYPE string VALUE 'ZSAT_I_DATABASETABLESANDVIEWS'.
  CONSTANTS zsat_i_databaseentityaggr TYPE string VALUE 'ZSAT_I_DATABASEENTITYAGGR'.
  CONSTANTS zsat_i_developmentpackage TYPE string VALUE 'ZSAT_I_DEVELOPMENTPACKAGE'.
  CONSTANTS zsat_p_cdsviewbase TYPE string VALUE 'ZSAT_P_CDSVIEWBASE'.
  CONSTANTS zsat_p_cds TYPE string VALUE 'ZSAT_P_CDS'.
  CONSTANTS zsat_i_cdsentity TYPE string VALUE 'ZSAT_I_CDSENTITY'.
  CONSTANTS zsat_i_cdsextensionviews TYPE string VALUE 'ZSAT_I_CDSEXTENSIONVIEWS'.
  CONSTANTS zsat_i_cdsviewt TYPE string VALUE 'ZSAT_I_CDSVIEWT'.
  CONSTANTS zsat_i_databaseview TYPE string VALUE 'ZSAT_I_DATABASEVIEW'.
  CONSTANTS zsat_i_ddlusageinddl TYPE string VALUE 'ZSAT_I_DDLUSAGEINDDL'.
  CONSTANTS zsat_i_cdsviewfield TYPE string VALUE 'ZSAT_I_CDSVIEWFIELD'.
  CONSTANTS zsat_i_cdsviewwithparameter TYPE string VALUE 'ZSAT_I_CDSVIEWWITHPARAMETER'.
  CONSTANTS zsat_i_cdsannotation TYPE string VALUE 'ZSAT_I_CDSANNOTATION'.
  CONSTANTS zsat_i_tablefield TYPE string VALUE 'ZSAT_I_TABLEFIELD'.
  CONSTANTS zsat_i_simpleclifmethod TYPE string VALUE 'ZSAT_I_SIMPLECLIFMETHOD'.
  CONSTANTS zsat_i_classinterface TYPE string VALUE 'ZSAT_I_CLASSINTERFACE'.
  CONSTANTS zsat_i_classinterfacet TYPE string VALUE 'ZSAT_I_CLASSINTERFACET'.
  CONSTANTS zsat_i_classflags TYPE string VALUE 'ZSAT_I_CLASSFLAGS'.
  CONSTANTS zsat_i_classattribute TYPE string VALUE 'ZSAT_I_CLASSATTRIBUTE'.
  CONSTANTS zsat_i_classinterfacemethod TYPE string VALUE 'ZSAT_I_CLASSINTERFACEMETHOD'.
  CONSTANTS zsat_i_classinterfacecomptext TYPE string VALUE 'ZSAT_I_CLASSINTERFACECOMPTEXT'.
  CONSTANTS zsat_i_clifmethodexception TYPE string VALUE 'ZSAT_I_CLIFMETHODEXCEPTION'.
  CONSTANTS zsat_i_clifmethodparam TYPE string VALUE 'ZSAT_I_CLIFMETHODPARAM'.
  CONSTANTS zsat_i_interfaceusage TYPE string VALUE 'ZSAT_I_INTERFACEUSAGE'.
  CONSTANTS zsat_i_superclass TYPE string VALUE 'ZSAT_I_SUPERCLASS'.
  CONSTANTS zsat_i_globalfriend TYPE string VALUE 'ZSAT_I_GLOBALFRIEND'.
  CONSTANTS zsat_i_reposource TYPE string VALUE 'ZSAT_I_REPOSOURCE'.
  CONSTANTS zsat_i_clifrelations TYPE string VALUE 'ZSAT_I_CLIFRELATIONS'.
ENDINTERFACE.
