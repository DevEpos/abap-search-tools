/*
 * View which returns all the information of the DDLS and STOB parts of a CDS view
 */
@AbapCatalog.sqlViewName: 'ZSATICDSEN'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entity in Data Definition'

define view ZSAT_I_CDSEntity
  with parameters
    p_language : abap.lang
  as select from    ZSAT_P_CDSViewBase as Base
    left outer join ddddlsrct          as Text on  Text.ddlname    = Base.DdlName
                                               and Text.ddlanguage = $parameters.p_language
{
  Base.EntityId,
  Base.RawEntityId,
  Base.DdlName,
  Base.ParentDdlName,
  case
    when Base.ParentDdlName <> '' then 'X' else ''
  end                    as IsExtend,
  Base.ViewName,
  Base.SourceType,
  Base.DdlSource,
  Base.DevelopmentPackage,
  $parameters.p_language as Language,
  Text.ddtext            as Description,
  Base.CreatedBy,
  Base.CreatedDate,
  Base.ChangedDate,
  'C'                    as Type
}
