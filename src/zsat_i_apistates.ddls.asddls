@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.sqlViewName: 'ZSATIAPST'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'API states'

define view ZSAT_I_ApiStates
  as select from SRIS_API_STATES_AGGREGATED

{
  key trobjtype                          as ObjectType,
  key cast(sobj_name as abap.char(30))   as ObjectName,

      cast(api_state as abap.char(30))   as FilterValue,
      cast('RELEASED' as abap.char(184)) as APIState
}
