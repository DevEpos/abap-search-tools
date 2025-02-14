@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATDBEAGG'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Aggr. Database Entities by package/type'

define view ZSAT_I_DatabaseEntityAggr
  as select from ZSAT_I_DatabaseEntity

{
  // ZSAT_I_DatabaseEntity
  Type,
  DevelopmentPackage,
  count(*)            as EntityCount
}

group by Type,
         DevelopmentPackage
