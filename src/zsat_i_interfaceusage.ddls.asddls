@AbapCatalog.sqlViewName: 'ZSATIINTFUSAGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface usage in Class/Interface'
define view ZSAT_I_InterfaceUsage
  as select from seometarel
{
  key clsname    as ClassName,
  key refclsname as UsedInterface
}
where
       version = '1'
  and(
       reltype = '0' // interface composition
    or reltype = '1' // interface implementation
  )
