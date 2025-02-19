@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATDTELTEXT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Text of DDIC Data Element'

@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_DataElementText
  as select from dd04t as Text

{
  key Text.rollname   as Rollname,
  key Text.ddlanguage as Language,

      Text.ddtext     as Description,
      Text.reptext    as RepText,
      Text.scrtext_l  as LongText,
      Text.scrtext_m  as MediumText,
      Text.scrtext_s  as ShortText
}

where Text.as4local = 'A'
