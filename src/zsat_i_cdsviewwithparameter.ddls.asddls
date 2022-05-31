@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View that has parameters'

define view entity ZSAT_I_CDSViewWithParameter
  as select distinct from dd10b
{
  key strucobjn as EntityId
}
where
  as4local = 'A'
