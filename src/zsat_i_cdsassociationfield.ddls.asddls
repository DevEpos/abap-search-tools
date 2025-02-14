@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSASSFLD'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'On Condition Field of Assoc. in CDS View'

define view ZSAT_I_CdsAssociationField
  as select from dd05b

{
  key strucobjn       as Entity,
  key associationname as AssociationName,
  key fieldname_t     as TargetField,
  key fdposition      as FieldPosition,

      fieldname       as SourceField,
      operator        as Operator,
      'AND'           as AndOrCondition
}

where as4local = 'A'
