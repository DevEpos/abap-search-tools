@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDBVIEW'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Database view'

define view ZSAT_I_DatabaseView
  with parameters
    p_language : abap.lang

  as select from    ZSAT_P_DatabaseView as DbView

    left outer join dd25t               as Text
      on  DbView.ViewName = Text.viewname
      and Text.ddlanguage = $parameters.p_language
      and Text.as4local   = 'A'

    left outer join dd25t               as FallBackText
      on  DbView.ViewName         = FallBackText.viewname
      and FallBackText.ddlanguage = DbView.OriginalLanguage
      and FallBackText.as4local   = 'A'

{
  DbView.ViewName,
  $parameters.p_language as Language,

  case
    when Text.ddtext is not null then Text.ddtext
    else FallBackText.ddtext
  end                    as Description,

  CreatedBy,
  CreatedDate,
  ChangedDate,
  ChangedBy,
  DevelopmentPackage,
  Type
}
