@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATDTELTEXT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Text of DDIC Data Element'

@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_DataElementText
  as select from dd04t as Text

{
  key Text.rollname         as Rollname,
  key Text.ddlanguage       as Language,

      Text.ddtext           as Description,
      upper(Text.ddtext)    as DescriptionUpper,
      Text.reptext          as RepText,
      upper(Text.reptext)   as RepTextUpper,
      Text.scrtext_l        as LongText,
      upper(Text.scrtext_l) as LongTextUpper,
      Text.scrtext_m        as MediumText,
      upper(Text.scrtext_m) as MediumTextUpper,
      Text.scrtext_s        as ShortText,
      upper(Text.scrtext_s) as ShortTextUpper
}

where Text.as4local = 'A'
