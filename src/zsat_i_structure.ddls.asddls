@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATISTRUCT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'DDIC Structure'

define view ZSAT_I_Structure
  as select from tadir as Repo

    inner join   dd02l as Struct on Struct.tabname = Repo.obj_name

{
  key Struct.tabname        as Structure,

      Struct.exclass        as ExtensionClass,
      Repo.author           as CreatedBy,
      Repo.created_on       as CreatedDate,
      Struct.as4date        as ChangedDate,
      Struct.as4user        as ChangedBy,
      Repo.devclass         as DevelopmentPackage,

      cast(case
        when Struct.tabclass = 'APPEND' then 'APPEND_STRUCT'
        else 'STRUCT'
      end as abap.char(15)) as Type
}

where (   Struct.tabclass = 'APPEND'
       or Struct.tabclass = 'INTTAB')
  and Struct.as4local = 'A'
  and Repo.pgmid      = 'R3TR'
  and Repo.object     = 'TABL'
