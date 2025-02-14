@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDBENT'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Database Entity like (View or Table)'

define view ZSAT_I_DatabaseEntity
  with parameters
    p_language : abap.lang

  as select from ZSAT_I_DatabaseTable(
                   p_language : $parameters.p_language)

{
  TableName           as Entity,
  TableName           as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}

union
  select from ZSAT_I_DatabaseView(
                p_language : $parameters.p_language)

{
  ViewName            as Entity,
  ViewName            as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}

union
  select from ZSAT_I_CdsEntity(
                p_language : $parameters.p_language)

{
  EntityId            as Entity,
  RawEntityId         as EntityRaw,
  Description,
  Language,
  DevelopmentPackage,
  CreatedBy,
  CreatedDate,
  ChangedBy,
  ChangedDate,
  Type
}

where IsExtend = ''
