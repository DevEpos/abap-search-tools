@AbapCatalog.sqlViewName: 'ZSATIDBFUSINCV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View Field used CDS View'

define view ZSAT_I_DbFieldUsedInCdsView
  with parameters
    P_BaseTable : tabname,
    P_BaseField : fieldname
  as select distinct from dd27s               as ViewField
    inner join            ZSAT_P_CdsViewBase  as CdsBase  on ViewField.viewname = CdsBase.ViewName
    inner join            ZSAT_I_CdsViewField as CdsField on  CdsBase.EntityId   = CdsField.EntityId
                                                          and CdsField.FieldName = ViewField.viewfield
    inner join            ZSAT_I_DdlApiState  as ApiState on CdsBase.DdlName = ApiState.DdlName
{
  key case
       when CdsBase.RawEntityId <> '' then CdsBase.RawEntityId
       else CdsBase.EntityId
      end as EntityId,
  key case
       when CdsField.RawFieldName <> '' then CdsField.RawFieldName
       else CdsField.FieldName
      end as FieldName,
      CdsBase.DdlName,
      CdsBase.SourceType,
      ApiState.ApiState
}
where
      ViewField.as4local    = 'A'
  and ViewField.tabname     = $parameters.P_BaseTable
  and ViewField.fieldname   = $parameters.P_BaseField
  and CdsBase.ParentDdlName = ''
