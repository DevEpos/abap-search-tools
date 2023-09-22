@AbapCatalog.sqlViewName: 'ZSATICLIFRELAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relations between Classes and/or Intf.'

define view ZSAT_I_ClIfRelations
  as select from seometarel
{
  key clsname    as Classname,
  key refclsname as RefClassname,
      reltype    as RelType
}
where
  version = '1'
