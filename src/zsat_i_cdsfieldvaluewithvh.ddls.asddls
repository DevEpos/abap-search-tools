/*
    Provides all views that have field annotations that
    can be used as value helps
*/
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICFVWVH'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'CDS View fields with assigned Value H.'

define view ZSAT_I_CdsFieldValueWithVH
  as select from ddfieldanno

{
  key strucobjn                as Entity,
  key lfieldname               as FieldName,

      value                    as AssociationNameRaw,
      replace(value, '''', '') as AssociationName
}

where name = 'OBJECTMODEL.FOREIGNKEY.ASSOCIATION'
   or name = 'CONSUMPTION.VALUEHELP'
