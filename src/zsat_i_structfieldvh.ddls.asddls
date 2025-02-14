@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATISTRCTFLDVH'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Field VH for Structure'

define view ZSAT_I_StructureFieldVh
  as select from dd03l as Field

    inner join   dd02l as Table
      on  Field.tabname  = Table.tabname
      and Table.as4local = 'A'

    inner join   tadir as Repo
      on  Repo.obj_name = Table.tabname
      and Repo.genflag  = ''
      and Repo.object   = 'TABL'

{
  key Field.tabname   as Structure,
  key Field.fieldname as FieldName
}

where Field.as4local = 'A'
  and (   Table.tabclass = 'INTTAB'
       or Table.tabclass = 'APPEND')
