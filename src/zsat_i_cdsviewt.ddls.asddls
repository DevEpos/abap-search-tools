@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSVT'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Text for CDS View'

@VDM.viewType: #BASIC

define view ZSAT_I_CdsViewT
  as select distinct from ddddlsrct as Text

{
  key Text.ddlname    as DdlName,
  key Text.ddlanguage as Language,

      Text.ddtext     as Description
}

where Text.as4local = 'A'
