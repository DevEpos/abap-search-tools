@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSASSHEAD'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Header of Association in CDS View'

define view ZSAT_I_CdsAssociationHeader
  as select from dd08b as Header

  association [1..*] to ZSAT_I_CdsAssociationField as _AssociationField
    on  Header.strucobjn       = _AssociationField.Entity
    and Header.associationname = _AssociationField.AssociationName

{
  key strucobjn       as SourceEntity,
  key associationname as AssociationName,
  key strucobjn_t     as TargetEntity,

      typekind_t      as TargetTypeKind,
      associationkind as AssociationKind,
      card_min        as CardinalityMin,
      card_max        as CardinalityMax,
      on_source       as OnSourceCondition,

      // Associations
      _AssociationField
}

where as4local = 'A'
