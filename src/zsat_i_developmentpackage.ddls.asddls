@AbapCatalog.sqlViewName: 'ZSATIDEVPACK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Development Package Information'

define view ZSAT_I_DevelopmentPackage
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    P_Language : abap.lang
  as select from    tdevc  as Package
    left outer join tdevct as Text on  Text.devclass = Package.devclass
                                   and Text.spras    = $session.system_language
{
  key Package.devclass   as DevelopmentPackage,
      Package.created_by as CreatedBy,
      Text.ctext         as Description
}
