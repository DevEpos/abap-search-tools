@AbapCatalog.sqlViewName: 'ZSATICDSVT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Text for CDS View'

@VDM.viewType: #BASIC

define view ZSAT_I_CDSViewT
  as select distinct from ddddlsrct    as FallbackText
    left outer join       ddddlsrc02bt as EndUserText on  EndUserText.ddlname    = FallbackText.ddlname
                                                      and EndUserText.as4local   = 'A'
                                                      and EndUserText.ddlanguage = FallbackText.ddlanguage
{
  key    FallbackText.ddlname    as DdlName,
  key    FallbackText.ddlanguage as Language,
         case
            when EndUserText.ddtext <> '' then EndUserText.ddtext
            else FallbackText.ddtext
         end                     as Description,
         FallbackText.ddtext     as FallbackDescription,
         EndUserText.ddtext      as EnduserDescription
}
