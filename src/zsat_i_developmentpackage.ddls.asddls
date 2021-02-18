@AbapCatalog.sqlViewName: 'ZSATIDEVPACK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Development Package Information'

define view ZSAT_I_DevelopmentPackage
  as select from    tdevc  as Package
    inner join      tadir  as Repo         on  Repo.obj_name = Package.devclass
                                           and Repo.object   = 'DEVC'
    left outer join tdevct as Text         on  Text.devclass = Package.devclass
                                           and Text.spras    = $session.system_language
    left outer join tdevct as FallbackText on  FallbackText.devclass = Package.devclass
                                           and FallbackText.spras    = Repo.masterlang
{
  key Package.devclass   as DevelopmentPackage,
      Package.created_by as CreatedBy,
      case
       when Text.ctext is null or Text.ctext = '' then FallbackText.ctext
       else Text.ctext
      end                as Description
}
