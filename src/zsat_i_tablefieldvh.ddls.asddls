@AbapCatalog.sqlViewName: 'ZSATITABFLDVH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Field in table'

define view ZSAT_I_TableFieldVh
  as select from dd03l as Field
    inner join   dd02l as Table on  Field.tabname  = Table.tabname
                                and Table.as4local = 'A'
    inner join   tadir as Repo  on  Repo.obj_name = Table.tabname
                                and Repo.genflag  = ''
                                and (
                                   Repo.object    = 'TABL'
                                   or Repo.object = 'VIEW'
                                 )
{
  key Field.tabname   as TableName,
  key Field.fieldname as FieldName,
      Field.rollname  as RollName,
      Table.tabclass  as TableClass
}
where
  Field.as4local = 'A'
