@AbapCatalog.sqlViewName: 'ZSATIDBFUSINV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Information about DB Field Usage'

define view ZSAT_I_DbFieldUsedInView
  with parameters
    P_BaseTable : tabname,
    P_BaseField : fieldname
  as select distinct from dd27s                as ViewField
    inner join       ZSAT_P_DatabaseView as DbView on ViewField.viewname = DbView.ViewName
{
  key ViewField.viewname,
  key ViewField.viewfield
}
where
      ViewField.tabname   = $parameters.P_BaseTable
  and ViewField.fieldname = $parameters.P_BaseField
  and ViewField.viewfield <> '-'
