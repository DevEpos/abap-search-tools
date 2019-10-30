@AbapCatalog.sqlViewName: 'ZSATIDBVIEW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database view'

define view ZSAT_I_DatabaseView
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    p_language : abap.lang
  as select from    ZSAT_P_DatabaseView as DbView
    left outer join dd25t                as Text         on  DbView.ViewName = Text.viewname
                                                         and Text.ddlanguage = $parameters.p_language
    left outer join dd25t                as FallBackText on  DbView.ViewName         = FallBackText.viewname
                                                         and FallBackText.ddlanguage = DbView.OriginalLanguage
{
  DbView.ViewName,
  $parameters.p_language as Language,
  case
    when Text.ddtext is not null then Text.ddtext
    else FallBackText.ddtext
  end                    as Description,
  case
    when Text.ddtext is not null then upper(Text.ddtext)
    else upper(FallBackText.ddtext)
  end                    as DescriptionUpper,
  CreatedBy,
  CreatedDate,
  ChangedDate,
  DevelopmentPackage,
  Type
}
