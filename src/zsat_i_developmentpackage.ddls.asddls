@AbapCatalog.sqlViewName: 'ZSATIDEVPACK'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Development Package Information'

define view ZSAT_I_DevelopmentPackage
  as select from tdevc as Package
    inner join   df14l as ApplComp on  Package.component = ApplComp.fctr_id
                                   and ApplComp.as4local = 'A'
{
  key Package.devclass  as DevelopmentPackage,
      Package.dlvunit   as SoftwareComponent,
      ApplComp.ps_posid as ApplicationComponent
}
