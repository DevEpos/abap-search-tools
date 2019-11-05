@AbapCatalog.sqlViewName: 'ZSATICDSASSFLD'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'On Condition Field of Assoc. in CDS View'

define view ZSAT_I_CDSAssociationField
  as select from dd05b
{
  key strucobjn                          as Entity,
  key associationname                    as AssociationName,
  key fieldname_t                        as TargetField,
  key fdposition                         as FieldPosition,
      fieldname                          as SourceField,
      operator                           as Operator,
      'AND'                              as AndOrCondition
}
where
  as4local = 'A'
