@AbapCatalog.sqlViewName: 'ZSATIDBTABAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DB Tables and DB views'

define view ZSAT_I_DatabaseTablesAndViews
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
