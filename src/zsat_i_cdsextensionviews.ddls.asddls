@AbapCatalog.sqlViewName: 'ZSATICDSEXTV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds Extension Views'

@VDM.viewType: #BASIC

define view ZSAT_I_CdsExtensionViews
  as select from    ddddlsrc             as source
    left outer join ZSAT_I_DdlDependency as dependency on source.ddlname = dependency.DdlName
{
  key source.ddlname    as DdlName,
      case
        when dependency.EntityName is null then source.ddlname
        else dependency.EntityName
      end               as EntityId,
      source.parentname as ParentDdl
}
where
      source.parentname <> ''
  and source.as4local   =  'A'
