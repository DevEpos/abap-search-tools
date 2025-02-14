@AbapCatalog.compiler.compareFilter: true
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

/* Change on 2019/10/10
 * -----------------------------
 * Show annotations of parameters
 */
union
  select from ddparameteranno

{
  strucobjn                as EntityId,
  parametername            as FieldName,
  name                     as Name,
  replace(value, '''', '') as Value
}
