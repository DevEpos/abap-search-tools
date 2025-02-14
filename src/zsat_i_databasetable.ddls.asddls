@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDBTAB'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Database Table'

define view ZSAT_I_DatabaseTable
  as select distinct from tadir as Repo

    inner join            dd02l as DbTable
      on DbTable.tabname = Repo.obj_name

    left outer join       dd02t as Text
      on  Text.tabname    = DbTable.tabname
      and Text.ddlanguage = $session.system_language
      and Text.as4local   = 'A'

    left outer join       dd02t as FallBackText
      on  FallBackText.tabname    = DbTable.tabname
      and FallBackText.ddlanguage = Repo.masterlang
      and FallBackText.as4local   = 'A'

{
  key DbTable.tabname          as TableName,

      DbTable.contflag         as DeliveryClass,
      DbTable.mainflag         as MaintenanceFlag,
      DbTable.shlpexi          as SearchHelpBindingExists,
      DbTable.clidep           as ClientDependent,
      DbTable.exclass          as ExtensionClass,
      DbTable.is_gtt           as IsGTT,
      $session.system_language as Language,

      case
        when Text.ddtext is not null then Text.ddtext
        else FallBackText.ddtext
      end                      as Description,

      case
        when Text.ddtext is not null then upper(Text.ddtext)
        else upper(FallBackText.ddtext)
      end                      as DescriptionUpper,

      author                   as CreatedBy,
      Repo.created_on          as CreatedDate,
      as4date                  as ChangedDate,
      as4user                  as ChangedBy,
      devclass                 as DevelopmentPackage,
      'T'                      as Type
}

where tabclass         = #tabclass.'TRANSP'
  and DbTable.as4local = 'A'
  and Repo.pgmid       = 'R3TR'
  and Repo.object      = 'TABL'
