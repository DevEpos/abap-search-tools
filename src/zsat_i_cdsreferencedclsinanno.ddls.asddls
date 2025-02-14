@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSREFCA'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Referenced clses in CDS views via Annot.'

define view ZSAT_I_CdsReferencedClsInAnno
  as select from ZSAT_I_CdsAnnotation

{
  EntityId,
  FieldName,
  Name,
  replace(Value, 'ABAP:', '') as Value
}

where Name like '.FILTER%.TRANSFORMEDBY'
  or Name like '.SORT%.TRANSFORMEDBY'
  or Name like '%.WRITEACTIVEIMPLEMENTEDBY'
  or Name like '%.VIRTUALELEMENTCALCULATEDBY'
  or Name like 'ANALYTICS%.READCLASSNAME'
  or Name like 'ANALYTICS%.WRITEBACK.CLASSNAME'
