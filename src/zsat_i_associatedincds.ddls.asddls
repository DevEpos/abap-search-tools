@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Entity which is associated in CDS View'

define view entity ZSAT_I_AssociatedInCDS
  as select from dd08b
{
  strucobjn   as DdlName,
  strucobjn_t as UsedEntity
}
where
  as4local = 'A'
