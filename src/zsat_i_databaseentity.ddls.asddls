@AbapCatalog.sqlViewName: 'ZSATIDBENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database Entity like (View or Table)'

define view ZSAT_I_DatabaseEntity
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    p_language : abap.lang
  as select from ZSAT_I_DatabaseTable(p_language : $parameters.p_language)
{
  TableName as Entity,
  TableName as EntityRaw,
  Description,
  DescriptionUpper,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedDate,
  Type
}
union select from ZSAT_I_DatabaseView(p_language : $parameters.p_language)
{
  ViewName as Entity,
  ViewName as EntityRaw,
  Description,
  DescriptionUpper,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedDate,
  Type
}
union select from ZSAT_I_CDSEntity(p_language : $parameters.p_language)
{
  EntityId    as Entity,
  RawEntityId as EntityRaw,
  Description,
  DescriptionUpper,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedDate,
  Type
}
