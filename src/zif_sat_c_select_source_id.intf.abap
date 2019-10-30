INTERFACE zif_sat_c_select_source_id
  PUBLIC .

  CONSTANTS:
    "! Table for DDL Sources
    ddddlsrc                      TYPE string VALUE 'DDDDLSRC',
    "! Table for Texts of DDL Sources
    ddddlsrct                     TYPE string VALUE 'DDDDLSRCT',
    "! Table for Enduser texts of DDL Sources
    ddddlscr02bt                  TYPE string VALUE 'DDDDLSRC02BT',
    "! Table for dependent objects of database views
    ddldependency                 TYPE string VALUE 'DDLDEPENDENCY',
    zsat_i_ddldependency          TYPE string VALUE 'ZSAT_I_DDLDEPENDENCY',
    "! Table for CDS Parameters
    dd10b                         TYPE string VALUE 'DD10B',
    zsat_i_associatedincds        TYPE string VALUE 'ZSAT_I_ASSOCIATEDINCDS',
    zsat_i_cdsfrompartentity      TYPE string VALUE 'ZSAT_I_CDSFROMPARTENTITY',
    zsat_i_databasetable          TYPE string VALUE 'ZSAT_I_DATABASETABLE',
    zsat_i_databaseentity         TYPE string VALUE 'ZSAT_I_DATABASEENTITY',
    zsat_i_databasetablesandviews TYPE string VALUE 'ZSAT_I_DATABASETABLESANDVIEWS',
    zsat_i_databaseentityaggr     TYPE string VALUE 'ZSAT_I_DATABASEENTITYAGGR',
    zsat_p_cdsviewbase            TYPE string VALUE 'ZSAT_P_CDSVIEWBASE',
    zsat_i_cdsentity              TYPE string VALUE 'ZSAT_I_CDSENTITY',
    zsat_i_cdsextensionviews      TYPE string VALUE 'ZSAT_I_CDSEXTENSIONVIEWS',
    zsat_i_cdsviewt               TYPE string VALUE 'ZSAT_I_CDSVIEWT',
    zsat_i_databaseview           TYPE string VALUE 'ZSAT_I_DATABASEVIEW',
    zsat_i_apistates              TYPE string VALUE 'ZSAT_I_APISTATES',
    zsat_i_ddlusageinddl          TYPE string VALUE 'ZSAT_I_DDLUSAGEINDDL',
    zsat_i_cdsviewfield           TYPE string VALUE 'ZSAT_I_CDSVIEWFIELD',
    zsat_i_cdsviewwithparameter   TYPE string VALUE 'ZSAT_I_CDSVIEWWITHPARAMETER',
    zsat_i_cdsannotation          TYPE string VALUE 'ZSAT_I_CDSANNOTATION',
    zsat_i_tablefield             TYPE string VALUE 'ZSAT_I_TABLEFIELD'.

ENDINTERFACE.
