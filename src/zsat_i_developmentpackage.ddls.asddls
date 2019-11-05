@AbapCatalog.sqlViewName: 'ZSATIDEVPACK'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Development Package Information'

define view ZSAT_I_DevelopmentPackage
  with parameters
    P_Language : abap.lang
  as select from    tdevc  as Package
    left outer join tdevct as Text         on  Text.devclass = Package.devclass
                                           and Text.spras    = $parameters.P_Language
{
  key Package.devclass   as DevelopmentPackage,
      Package.created_by as CreatedBy,
      Text.ctext         as Description
}
