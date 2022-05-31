@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database view'

define view entity ZSAT_I_DatabaseView
  as select from    ZSAT_P_DatabaseView as DbView
    left outer join dd25t               as Text         on  DbView.ViewName = Text.viewname
                                                        and Text.ddlanguage = $session.system_language
    left outer join dd25t               as FallBackText on  DbView.ViewName         = FallBackText.viewname
                                                        and FallBackText.ddlanguage = DbView.OriginalLanguage
{
  DbView.ViewName,
  cast( '' as contflag )   as DeliveryClass,
  $session.system_language as Language,
  case
    when Text.ddtext is not null then Text.ddtext
    else FallBackText.ddtext
  end                      as Description,
  case
    when Text.ddtext is not null then upper(Text.ddtext)
    else upper(FallBackText.ddtext)
  end                      as DescriptionUpper,
  DbView.CreatedBy,
  DbView.CreatedDate,
  DbView.ChangedDate,
  DbView.ChangedBy,
  DbView.DevelopmentPackage,
  DbView.Type
}
