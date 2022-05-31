@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Super class releationship'

define view entity ZSAT_I_SuperClass
  as select from seometarel
{
      //seometarel
  key clsname    as ClassName,
  key refclsname as SuperClass
}
where
      version = '1'
  and reltype = '2' // inheritence
