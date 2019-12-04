@AbapCatalog.sqlViewName: 'ZSATIAPST'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'API states'

define view ZSAT_I_APIStates
  as select from SRIS_API_STATES_AGGREGATED
{
  key trobjtype                            as ObjectType,
  key cast( sobj_name as abap.char(30) )   as ObjectName,
      cast( api_state as abap.char(30) )   as FilterValue,
      cast( 'RELEASED' as abap.char(184) ) as APIState
}
