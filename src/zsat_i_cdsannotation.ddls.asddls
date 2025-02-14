@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSANNO'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Annotations for CDS Views'

define view ZSAT_I_CdsAnnotation
  as select from ddfieldanno

{
  strucobjn                as EntityId,
  lfieldname               as FieldName,
  name                     as Name,
  replace(value, '''', '') as Value
}

union
  select from ddheadanno

{
  strucobjn                as EntityId,
  ''                       as FieldName,
  name                     as Name,
  replace(value, '''', '') as Value
}
