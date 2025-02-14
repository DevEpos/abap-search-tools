@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.sqlViewName: 'ZSATIAPST'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'API states'

define view ZSAT_I_ApiStates
  as select distinct from ZSAT_P_ArsApis as api

{
  key api.ObjectType,
  key cast(api.ObjectName as char30)     as ObjectName,

  key cast(case
        when (api.Attribute = 'RELEASE_STATE' and api.Value = 'RELEASED_PUBLIC') then 'PUBLIC'
        when (api.Attribute = 'RELEASE_STATE' and api.Value = '2')               then 'KEY_USER'
        when (api.Attribute = 'RELEASE_STATE' and api.Value = '3')               then 'CLOUD'
        else 'NOT_RELEASED'
      end as char30)                     as FilterValue,

      cast('RELEASED' as abap.char(184)) as APIState
}

where (    api.Attribute = 'RELEASE_STATE'
       and (api.Value = '2' or api.Value = '3' or api.Value = 'RELEASED_PUBLIC'))
