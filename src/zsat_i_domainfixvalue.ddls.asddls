@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDOMFIXV'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Domain fix values'

define view ZSAT_I_DomainFixValue
  as select from    dd07l as FixedValue

    left outer join dd07t as ValueText
      on  FixedValue.domname    = ValueText.domname
      and FixedValue.domvalue_l = ValueText.domvalue_l
      and FixedValue.as4local   = ValueText.as4local

{
  key FixedValue.domname      as DomainName,
  key FixedValue.domvalue_l   as Low,
  key ValueText.ddlanguage    as Language,

      ValueText.ddtext        as Text,
      upper(ValueText.ddtext) as TextUpper
}

where FixedValue.as4local = 'A' -- Active
