@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds Extension Views'

@VDM.viewType: #BASIC

define view entity ZSAT_I_CdsExtensionViews
  as select from ddddlsrc             as source
    inner join   ZSAT_I_DdlDependency as dependency on source.ddlname = dependency.DdlName
{
  source.ddlname        as DdlName,
  dependency.EntityName as EntityId,
  source.parentname     as ParentDdl
}
where
      source.parentname <> ''
  and source.as4local   =  'A'
