@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDBFUSINV'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Information about DB Field Usage'

define view ZSAT_I_DbFieldUsedInView
  with parameters
    P_BaseTable : tabname,
    P_BaseField : fieldname

  as select distinct from dd27s               as ViewField

    inner join            ZSAT_P_DatabaseView as DbView    on ViewField.viewname = DbView.ViewName

{
  key ViewField.viewname,
  key ViewField.viewfield
}

where ViewField.as4local   = 'A'
  and ViewField.tabname    = $parameters.P_BaseTable
  and ViewField.fieldname  = $parameters.P_BaseField
  and ViewField.viewfield <> '-'
