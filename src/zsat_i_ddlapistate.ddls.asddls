@AbapCatalog.sqlViewName: 'ZSATIDDLAPIST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'API State of Data Definitions'

define view ZSAT_I_DdlApiState
  as select distinct from ddddlsrc         as ddl
    left outer join       ZSAT_I_ApiStates as rel  on  ddl.ddlname    = rel.ObjectName
                                                   and rel.ObjectType = 'DDLS'
                                                   and rel.APIState   = 'RELEASED'
    left outer join       ZSAT_I_ApiStates as depr on  ddl.ddlname     = depr.ObjectName
                                                   and depr.ObjectType = 'DDLS'
                                                   and depr.APIState   = 'DEPRECATED'
{
  key ddl.ddlname as DdlName,
      case
        when rel.APIState is not null and depr.APIState is not null then 'RELEASED;DEPRECATED'
        when rel.APIState is not null then 'RELEASED'
        when depr.APIState is not null then 'DEPRECATED'
        else ''
      end         as ApiState
}
