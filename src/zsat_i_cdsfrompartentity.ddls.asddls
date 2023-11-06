@AbapCatalog.sqlViewName: 'ZSATICDSFPE'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entities in Select From clause of CDS'

-- CDS views which have Database tables in select part
define view ZSAT_I_CdsFromPartEntity
  as select from dd26s as BaseTable
    inner join   dd02l as DbTable on  BaseTable.tabname = DbTable.tabname
                                  and DbTable.tabclass  = 'TRANSP'
                                  and DbTable.as4local  = 'A'
{
  BaseTable.viewname as DdlViewName,
  BaseTable.tabname  as SourceEntity
}
where
      BaseTable.as4local = 'A'
  and BaseTable.tabname  not like 'DDDDL%TYPES'
-- CDS views which have Database Views (not DDL Views of CDS views) in select part
union select from dd26s as BaseTable
  inner join      tadir as Repo on  BaseTable.tabname = Repo.obj_name
                                and Repo.object       = 'VIEW'
                                and Repo.genflag      = ''
{
  BaseTable.viewname as DdlViewName,
  BaseTable.tabname  as SourceEntity
}
where
  BaseTable.as4local = 'A'
-- CDS views which have other CDS views in the select part
union select from dd26s                as BaseTable
  inner join      ZSAT_I_DdlDependency as DdlMap on BaseTable.tabname = DdlMap.ViewName
{
  BaseTable.viewname as DdlViewName,
  DdlMap.EntityName  as SourceEntity
}
where
  BaseTable.as4local = 'A'
-- CDS views which have table functions in the select part
union select from ZSAT_I_CdsBaseTable as BaseTable
{
  BaseTable.DdlView   as DdlViewName,
  BaseTable.BaseTable as SourceEntity
}
where
  BaseTable.TadirType = 'STOB'
