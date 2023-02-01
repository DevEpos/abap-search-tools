@AbapCatalog.sqlViewName: 'ZSATIFORKEYTAB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database tables of for. key definition'

define view ZSAT_I_ForeignKeyTable
  as select distinct from dd08l                as ForeignKey
    left outer join       ZSAT_I_DatabaseTable as DatabaseTable on ForeignKey.checktable = DatabaseTable.TableName
{
  key ForeignKey.tabname    as TableName,
  key ForeignKey.checktable as ForeignKeyTable,
      DatabaseTable.CreatedBy,
      DatabaseTable.DevelopmentPackage,
      DatabaseTable.Description
}
where
  ForeignKey.as4local = 'A'
