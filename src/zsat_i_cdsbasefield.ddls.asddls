@AbapCatalog.sqlViewName: 'ZSATICDSBASEFLD'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base Field of CDS View'
@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_CdsBaseField
  as select distinct from dd27s as BaseField
    inner join            ddldependency as Dependency on  BaseField.viewname    = Dependency.objectname
                                                      and Dependency.objecttype = 'VIEW'
                                                      and Dependency.state      = 'A'
{
  BaseField.viewname  as ViewName,
  BaseField.viewfield as ViewField,
  BaseField.tabname   as BaseTable,
  BaseField.fieldname as FieldName
}
where
      BaseField.as4local  = 'A'
  and BaseField.fieldname <> '' // to exclude fields from appends      
  and BaseField.fieldname <> 'MANDT'

union all select from zsatcds2mfield
{
  cast( ddlname as viewname ) as ViewName,
  viewfield                   as ViewField,
  basetable                   as BaseTable,
  fieldname                   as FieldName
}
