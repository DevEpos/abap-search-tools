@AbapCatalog.sqlViewName: 'ZSATIASINCDS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Entity which is associated in CDS View'

define view ZSAT_I_AssociatedInCDS
  as select from dd08b
{
  strucobjn   as DdlName,
  strucobjn_t as UsedEntity
}
where
  as4local = 'A'
