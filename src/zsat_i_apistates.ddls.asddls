@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.sqlViewName: 'ZSATIAPST'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'API states'

define view ZSAT_I_ApiStates
  as select from ARS_ADT_API_FILTER

{
  key tadir_object                          as ObjectType,
  key cast(tadir_obj_name as abap.char(30)) as ObjectName,
  key filter_value                          as FilterValue,

      state                                 as APIState
}
