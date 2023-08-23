@AbapCatalog.sqlViewName: 'ZSATITABFLD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Field in table'

define view ZSAT_I_TableField
  as select distinct from dd03l as Field
{
  key Field.tabname   as TableName,
  key Field.fieldname as FieldName,
      Field.rollname  as RollName,
      Field.domname   as DomainName,
      Field.precfield as IncludeName
}
where
  Field.as4local = 'A'
