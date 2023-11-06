@AbapCatalog.sqlViewName: 'ZSATIDBENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database Entity like (View or Table)'

define view ZSAT_I_DatabaseEntity
  as select from ZSAT_I_DatabaseTable
{
  TableName as Entity,
  TableName as EntityRaw,
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
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}
union select from ZSAT_I_CdsEntity
{
  EntityId    as Entity,
  RawEntityId as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}
where
  IsExtend = ''
