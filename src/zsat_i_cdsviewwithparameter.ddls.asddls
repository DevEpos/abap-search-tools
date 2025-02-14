@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSVWP'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'CDS View that has parameters'

define view ZSAT_I_CdsViewWithParameter
  as select distinct from dd10b

{
  key strucobjn as EntityId
}

where as4local = 'A'
