@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATPDBVIEW'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Information about Database view'

define view ZSAT_P_DatabaseView
  as select from tadir as Repo

    inner join   dd25l as DbView
      on  Repo.obj_name = DbView.viewname
      and Repo.pgmid    = 'R3TR'
      and Repo.object   = 'VIEW'

{
  DbView.viewname as ViewName,
  Repo.author     as CreatedBy,
  Repo.created_on as CreatedDate,
  DbView.as4date  as ChangedDate,
  DbView.as4user  as ChangedBy,
  Repo.devclass   as DevelopmentPackage,
  Repo.masterlang as OriginalLanguage,
  'V'             as Type
}

where DbView.as4local = 'A'
  and Repo.genflag    = ' '
  and (   DbView.viewclass = ' '
       or DbView.viewclass = 'D')
