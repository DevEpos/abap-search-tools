@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base Field of CDS View'
@Metadata.ignorePropagatedAnnotations: true

define view entity ZSAT_I_CdsBaseField
  as select distinct from dd27s as BaseField
    inner join            tadir as Repo on  BaseField.viewname = Repo.obj_name
                                        and Repo.object        = 'VIEW'
                                        and Repo.genflag       = 'X'
{
  BaseField.viewname  as ViewName,
  BaseField.viewfield as ViewField,
  BaseField.tabname   as BaseTable,
  BaseField.fieldname as FieldName
}
where
  BaseField.fieldname <> 'MANDT'

union all select from zsatcds2mfield
{
  cast( ddlname as viewname ) as ViewName,
  viewfield                   as ViewField,
  basetable                   as BaseTable,
  fieldname                   as FieldName
}
