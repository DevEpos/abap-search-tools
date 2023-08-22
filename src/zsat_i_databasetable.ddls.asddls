@AbapCatalog.sqlViewName: 'ZSATIDBTAB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Database Table'

define view ZSAT_I_DatabaseTable
  as select distinct from tadir as Repo
    inner join            dd02l as DbTable on DbTable.tabname  = Repo.obj_name
    left outer join       dd02t as Text    on  Text.tabname    = DbTable.tabname
                                           and Text.ddlanguage = $session.system_language
{
  key DbTable.tabname          as TableName,
      DbTable.contflag         as DeliveryClass,
      DbTable.mainflag         as MaintenanceFlag,
      DbTable.shlpexi          as SearchHelpBindingExists,
      DbTable.clidep           as ClientDependent,
      DbTable.exclass          as ExtensionClass,
      DbTable.is_gtt           as IsGTT,
      $session.system_language as Language,
      Text.ddtext              as Description,
      author                   as CreatedBy,
      Repo.created_on          as CreatedDate,
      as4date                  as ChangedDate,
      as4user                  as ChangedBy,
      devclass                 as DevelopmentPackage,
      'T'                      as Type
}
where
      tabclass         = #tabclass.'TRANSP'
  and DbTable.as4local = 'A'
  and Repo.pgmid       = 'R3TR'
  and Repo.object      = 'TABL'
