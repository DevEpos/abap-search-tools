@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Association Headers assngd. to CDS flds.'

define view entity ZSAT_I_CDSFieldVHAssocHeader
  as select from ZSAT_I_CDSFieldValueWithVH as ValueHelpField
  association [1..*] to ZSAT_I_CDSAssociationHeader as _AssociationHeader on  ValueHelpField.Entity          = _AssociationHeader.SourceEntity
                                                                           and ValueHelpField.AssociationName = _AssociationHeader.AssociationName
{
      //ValueHelpField
  key Entity,
  key FieldName,
      AssociationNameRaw,
      AssociationName,

      // Associations
      _AssociationHeader
}
