@AbapCatalog.sqlViewName: 'ZSATICDSEXTV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds Extension Views'

@VDM.viewType: #BASIC

define view ZSAT_I_CdsExtensionViews
  as select from ddddlsrc             as source
    inner join   ZSAT_I_DdlDependency as dependency on source.ddlname = dependency.DdlName
{
  source.ddlname        as DdlName,
  dependency.EntityName as EntityId,
  source.parentname     as ParentDdl
}
where
      parentname <> ''
  and as4local   =  'A'
