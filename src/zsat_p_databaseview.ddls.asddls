@AbapCatalog.sqlViewName: 'ZSATPDBVIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Information about Database view'

define view ZSAT_P_DatabaseView
  as select from tadir as Repo
    inner join   dd25l as DbView on  Repo.obj_name = DbView.viewname
                                 and Repo.pgmid    = 'R3TR'
                                 and Repo.object   = 'VIEW'
{
  DbView.viewname        as ViewName,
  author                 as CreatedBy,
  Repo.created_on        as CreatedDate,
  as4date                as ChangedDate,
  devclass               as DevelopmentPackage,
  Repo.masterlang        as OriginalLanguage,
  'V'                    as Type
}
where
       DbView.as4local = 'A'
  and  genflag         = #genflag.' '
  and(
       viewclass       = #viewclass.' '
    or viewclass       = #viewclass.'D'
  )
