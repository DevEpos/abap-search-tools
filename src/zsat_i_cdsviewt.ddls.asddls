@AbapCatalog.sqlViewName: 'ZSATICDSVT'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Text for CDS View'

@VDM.viewType: #BASIC

define view ZSAT_I_CdsViewT
  as select distinct from ddddlsrct    as Text
{
  key    Text.ddlname    as DdlName,
  key    Text.ddlanguage as Language,
         Text.ddtext     as Description
}
where
  FallbackText.as4local = 'A'
