@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Aggr. Database Entities by package/type'

define view entity ZSAT_I_DatabaseEntityAggr
  as select from ZSAT_I_DatabaseEntity
{
  //ZSAT_I_DatabaseEntity
  Type,
  DevelopmentPackage,
  count(*) as EntityCount
}
group by
  Type,
  DevelopmentPackage
