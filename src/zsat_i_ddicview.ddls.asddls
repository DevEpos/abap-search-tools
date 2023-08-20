@AbapCatalog.sqlViewName: 'ZSATIDDICVIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dictionary View'

define view ZSAT_I_DdicView
  as select from    tadir as Repo
    inner join      dd25l as DbView       on  Repo.obj_name = DbView.viewname
                                          and Repo.pgmid    = 'R3TR'
                                          and Repo.object   = 'VIEW'
    left outer join dd25t as Text         on  DbView.viewname = Text.viewname
                                          and Text.ddlanguage = $session.system_language
    left outer join dd25t as FallBackText on  DbView.viewname         = FallBackText.viewname
                                          and FallBackText.ddlanguage = Repo.masterlang
{
  key DbView.viewname          as ViewName,
      DbView.viewclass         as ViewClass,
      DbView.globalflag        as MaintenanceFlag,
      DbView.customauth        as DeliveryClass,
      DbView.roottab           as PrimaryTable,
      devclass                 as DevelopmentPackage,
      $session.system_language as Language,
      case
        when Text.ddtext is not null then Text.ddtext
        else FallBackText.ddtext
      end                      as Description,
      case
        when Text.ddtext is not null then upper(Text.ddtext)
        else upper(FallBackText.ddtext)
      end                      as DescriptionUpper,
      author                   as CreatedBy,
      Repo.created_on          as CreatedDate,
      as4date                  as ChangedDate,
      as4user                  as ChangedBy,
      Repo.masterlang          as OriginalLanguage
}
where
      DbView.as4local = 'A'
  and genflag         = #genflag.' '
  and aggtype         = #aggtype.'V'
