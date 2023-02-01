/*
 * View which returns all the information of the DDLS and STOB parts of a CDS view
 */
@AbapCatalog.sqlViewName: 'ZSATICDSEN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Entity in Data Definition'

define view ZSAT_I_CDSEntity
  as select from    ZSAT_P_CDSViewBase as Base
    left outer join ddddlsrc02bt       as Text         on  Text.ddlname    = Base.DdlName
                                                       and Text.strucobjn  = Base.EntityId
                                                       and Text.ddlanguage = $session.system_language
    left outer join ddddlsrct          as FallbackText on  FallbackText.ddlname    = Base.DdlName
                                                       and FallbackText.ddlanguage = Base.OriginalLanguage
  association [0..*] to ZSAT_I_APIStates as _ApiState on  Base.DdlName          =  _ApiState.ObjectName
                                                      and _ApiState.ObjectType  =  'DDLS'
                                                      and _ApiState.FilterValue <> 'ADD_CUSTOM_FIELDS'
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
  case
    when Text.ddtext is null or Text.ddtext = '' then upper( FallbackText.ddtext )
    else upper( Text.ddtext )
  end                      as DescriptionUpper,
  Base.CreatedBy,
  Base.CreatedDate,
  Base.ChangedBy,
  Base.ChangedDate,
  'C'                      as Type,
  _ApiState
}
