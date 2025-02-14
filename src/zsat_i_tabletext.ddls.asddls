@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATITABTEXT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Text of TABL'

define view ZSAT_I_TableText
  as select from dd02t as TableText

{
  key TableText.tabname    as TableName,
  key TableText.ddlanguage as Language,

      TableText.ddtext     as Description
}

where TableText.as4local = 'A'
