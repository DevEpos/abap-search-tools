@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface usage in Class/Interface'

define view entity ZSAT_I_InterfaceUsage
  as select from seometarel
{
  key clsname    as ClassName,
  key refclsname as UsedInterface
}
where
      version = '1'
  and reltype = '1' // interface 
