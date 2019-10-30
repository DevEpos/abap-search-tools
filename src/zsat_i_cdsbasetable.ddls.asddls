@AbapCatalog.sqlViewName: 'ZSATICDSBASET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Base Table of a CDS View'
/*
 * Represents a Base Table of CDS view
 */
define view ZSAT_I_CdsBaseTable
  as select from dd26s as BaseTable
    inner join   tadir as Repo on BaseTable.tabname = Repo.obj_name
{
  BaseTable.viewname as DdlView,
  BaseTable.tabname  as BaseTable,
  BaseTable.tabpos   as TablePosition,
  Repo.object        as TadirType,
  case
    when Repo.object = 'TABL' then 'T'
    when Repo.object = 'VIEW' then 'V'
    when Repo.object = 'STOB' then 'C'
  end                as EntityType,
  Repo.genflag       as GenerationFlag
}
where
     Repo.object = 'VIEW'
  or Repo.object = 'STOB'
  or Repo.object = 'TABL'
