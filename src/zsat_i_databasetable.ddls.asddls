@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDBTAB'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Database Table'

define view ZSAT_I_DatabaseTable
  with parameters
    p_language : abap.lang

  as select distinct from tadir as Repo

    inner join            dd02l as DbTable
      on DbTable.tabname = Repo.obj_name

    left outer join       dd02t as Text
      on  Text.tabname    = DbTable.tabname
      and Text.ddlanguage = $parameters.p_language
      and Text.as4local   = 'A'

{
  key DbTable.tabname        as TableName,

      DbTable.contflag       as DeliveryClass,
      DbTable.mainflag       as MaintenanceFlag,
      DbTable.shlpexi        as SearchHelpBindingExists,
      DbTable.clidep         as ClientDependent,
      DbTable.exclass        as ExtensionClass,
      $parameters.p_language as Language,
      Text.ddtext            as Description,
      author                 as CreatedBy,
      Repo.created_on        as CreatedDate,
      as4date                as ChangedDate,
      as4user                as ChangedBy,
      devclass               as DevelopmentPackage,
      'T'                    as Type
}

where tabclass         = 'TRANSP'
  and DbTable.as4local = 'A'
  and Repo.pgmid       = 'R3TR'
  and Repo.object      = 'TABL'
