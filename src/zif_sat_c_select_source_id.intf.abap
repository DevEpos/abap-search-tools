INTERFACE zif_sat_c_select_source_id
  PUBLIC .

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Table for DDL Sources</p>
    ddddlsrc                      TYPE string VALUE 'DDDDLSRC',
    "! <p class="shorttext synchronized" lang="en">Table for Texts of DDL Sources</p>
    ddddlsrct                     TYPE string VALUE 'DDDDLSRCT',
    "! <p class="shorttext synchronized" lang="en">Table for Enduser texts of DDL Sources</p>
    ddddlscr02bt                  TYPE string VALUE 'DDDDLSRC02BT',
    "! <p class="shorttext synchronized" lang="en">Table for dependent objects of database views</p>
    ddldependency                 TYPE string VALUE 'DDLDEPENDENCY',
    zsat_i_ddldependency          TYPE string VALUE 'ZSAT_I_DDLDEPENDENCY',
    "! <p class="shorttext synchronized" lang="en">Table for CDS Parameters</p>
    dd10b                         TYPE string VALUE 'DD10B',
    " <p class="shorttext synchronized" lang="en">Table for CDS Association Header</p>
    dd08b                         TYPE string VALUE 'DD08B',
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
    zsat_i_tablefield             TYPE string VALUE 'ZSAT_I_TABLEFIELD',
    zsat_i_classinterface         TYPE string VALUE 'ZSAT_I_CLASSINTERFACE',
    zsat_i_classinterfacet        TYPE string VALUE 'ZSAT_I_CLASSINTERFACET',
    zsat_i_classflags             TYPE string VALUE 'ZSAT_I_CLASSFLAGS',
    zsat_i_classattribute         TYPE string VALUE 'ZSAT_I_CLASSATTRIBUTE',
    zsat_i_classmethod            TYPE string VALUE 'ZSAT_I_CLASSMETHOD',
    zsat_i_interfaceusage         TYPE string VALUE 'ZSAT_I_INTERFACEUSAGE',
    zsat_i_superclass             TYPE string VALUE 'ZSAT_I_SUPERCLASS',
    zsat_i_globalfriend           TYPE string VALUE 'ZSAT_I_GLOBALFRIEND'.
ENDINTERFACE.
