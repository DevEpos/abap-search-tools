/*
 * View which returns all the information of the DDLS and STOB parts of a CDS view
 */
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSEN'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Entity in Data Definition'

define view ZSAT_I_CdsEntity
  as select from    ZSAT_P_CdsViewBase as Base

    left outer join ddddlsrc02bt       as Text
      on  Text.ddlname    = Base.DdlName
      and Text.strucobjn  = Base.EntityId
      and Text.ddlanguage = $session.system_language
      and Text.as4local   = 'A'

    left outer join ddddlsrct          as FallbackText
      on  FallbackText.ddlname    = Base.DdlName
      and FallbackText.ddlanguage = Base.OriginalLanguage
      and FallbackText.as4local   = 'A'

  association [0..*] to ZSAT_I_ApiStates as _ApiState
    on  Base.DdlName         = _ApiState.ObjectName
    and _ApiState.ObjectType = 'DDLS'

{
  Base.EntityId,
  Base.RawEntityId,
  Base.DdlName,
  Base.ParentDdlName,

  case
    when Base.ParentDdlName <> '' then 'X' else ''
  end                      as IsExtend,

  Base.ViewName,
  Base.SourceType,
  Base.DdlSource,
  Base.DevelopmentPackage,
  $session.system_language as Language,

  case
    when Text.ddtext is null or Text.ddtext = '' then FallbackText.ddtext
    else Text.ddtext
  end                      as Description,

  Base.CreatedBy,
  Base.CreatedDate,
  Base.ChangedBy,
  Base.ChangedDate,
  'C'                      as Type,

  _ApiState
}
