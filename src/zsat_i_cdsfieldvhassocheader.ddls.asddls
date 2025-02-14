@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICFVHASOCH'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Association Headers assngd. to CDS flds.'

define view ZSAT_I_CdsFieldVhAssocHeader
  as select from ZSAT_I_CdsFieldValueWithVH as ValueHelpField

  association [1..*] to ZSAT_I_CdsAssociationHeader as _AssociationHeader
    on  ValueHelpField.Entity          = _AssociationHeader.SourceEntity
    and ValueHelpField.AssociationName = _AssociationHeader.AssociationName

{
      // ValueHelpField
  key Entity,
  key FieldName,

      AssociationNameRaw,
      AssociationName,

      // Associations
      _AssociationHeader
}
