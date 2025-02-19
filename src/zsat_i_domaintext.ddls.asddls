@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDOMATEXT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Text of DDIC Domain'

@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_Domaintext
  as select from dd01t as Text

{
  key Text.domname       as DomainName,
  key Text.ddlanguage    as Language,

      Text.ddtext        as Description,
      upper(Text.ddtext) as DescriptionUpper
}

where Text.as4local = 'A'
