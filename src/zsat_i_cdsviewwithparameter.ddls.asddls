@AbapCatalog.sqlViewName: 'ZSATICDSVWP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS View that has parameters'

define view ZSAT_I_CdsViewWithParameter
  as select distinct from dd10b
{
  key strucobjn as EntityId
}
where
  as4local = 'A'
