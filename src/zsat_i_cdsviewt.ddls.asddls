@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSVT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Text for CDS View'

@VDM.viewType: #BASIC

define view ZSAT_I_CdsViewT
  as select distinct from ddddlsrct    as FallbackText

    left outer join       ddddlsrc02bt as EndUserText
      on  EndUserText.ddlname    = FallbackText.ddlname
      and EndUserText.as4local   = 'A'
      and EndUserText.ddlanguage = FallbackText.ddlanguage

{
  key FallbackText.ddlname    as DdlName,
  key FallbackText.ddlanguage as Language,

      case
         when EndUserText.ddtext <> '' then EndUserText.ddtext
         else FallbackText.ddtext
      end                     as Description,

      case
         when EndUserText.ddtext <> '' then upper(EndUserText.ddtext)
         else upper(FallbackText.ddtext)
      end                     as DescriptionUpper,

      FallbackText.ddtext     as FallbackDescription,
      EndUserText.ddtext      as EnduserDescription
}

where FallbackText.as4local = 'A'
