@AbapCatalog.sqlViewName: 'ZSATIDBTABAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DB Tables and DB views'

define view ZSAT_I_DatabaseTablesAndViews
  as select from ZSAT_I_DatabaseTable
{
  TableName as Entity,
  TableName as EntityRaw,
  DeliveryClass,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}
union select from ZSAT_I_DatabaseView
{
  ViewName as Entity,
  ViewName as EntityRaw,
  DeliveryClass,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}
