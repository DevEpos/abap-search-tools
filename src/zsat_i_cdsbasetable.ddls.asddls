@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSBASET'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Base Table of a CDS View'

/*
 * Represents a Base Table of CDS view
 */
define view ZSAT_I_CdsBaseTable
  as select from dd26s as BaseTable

    inner join   tadir as Repo      on BaseTable.tabname = Repo.obj_name

{
  BaseTable.viewname as DdlView,
  BaseTable.tabname  as BaseTable,
  BaseTable.tabpos   as TablePosition,
  Repo.object        as TadirType,

  case
    when Repo.object = 'TABL' then 'T'
    when Repo.object = 'VIEW' then 'V'
    when Repo.object = 'STOB' then 'C'
  end                as EntityType,

  Repo.genflag       as GenerationFlag
}

where BaseTable.as4local = 'A'
  and (   Repo.object = 'VIEW'
       or Repo.object = 'STOB'
       or Repo.object = 'TABL')

union all
  select from  zsatcds2mbtab as BaseTable

    inner join tadir         as Repo      on BaseTable.basetable = Repo.obj_name

{
  BaseTable.ddlname   as DdlView,
  BaseTable.basetable as BaseTable,
  cast('' as mcpos)   as TablePosition,
  Repo.object         as TadirType,

  case
    when Repo.object = 'TABL' then 'T'
    when Repo.object = 'VIEW' then 'V'
    when Repo.object = 'STOB' then 'C'
  end                 as EntityType,

  Repo.genflag        as GenerationFlag
}

where Repo.object = 'VIEW'
   or Repo.object = 'STOB'
   or Repo.object = 'TABL'
