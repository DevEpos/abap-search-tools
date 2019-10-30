@AbapCatalog.sqlViewName: 'ZSATIABAPQUS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'ABAP Quick Search'

define view ZSAT_I_RisQuickSearch
  // CDS Views
  as select from ZSAT_P_CDSViewBase
{
  'STOB'                     as ObjectType,
  DdlName                    as ParentObjectName,
  cast( EntityId as char40 ) as ObjectName,
  RawEntityId                as RawObjectName,
  CreatedBy,
  DevelopmentPackage,
  'STOB'                     as AliasType
}
union all
// executable programs
select distinct from tadir
  inner join         progdir on  tadir.obj_name = progdir.name
                             and tadir.object   = 'PROG'
{
  tadir.object   as ObjectType,
  tadir.obj_name as ParentObjectName,
  tadir.obj_name as ObjectName,
  ''             as RawObjectName,
  tadir.author   as CreatedBy,
  tadir.devclass as DevelopmentPackage,
  'REPO'         as AliasType
}
where
  (
        tadir.pgmid   =  'R3TR'
    and tadir.delflag <> 'X'
    and progdir.subc  =  '1'
  )

union all

// program includes
select distinct from tadir
  inner join         progdir on  tadir.obj_name = progdir.name
                             and tadir.object   = 'PROG'
{
  tadir.object   as ObjectType,
  tadir.obj_name as ParentObjectName,
  tadir.obj_name as ObjectName,
  ''             as RawObjectName,
  tadir.author   as CreatedBy,
  tadir.devclass as DevelopmentPackage,
  'INCL'         as AliasType
}
where
  (
        tadir.pgmid   =  'R3TR'
    and tadir.delflag <> 'X'
    and progdir.subc  =  'I'
  )

union all

select distinct from tadir
  inner join         progdir on  tadir.obj_name = progdir.name
                             and tadir.object   = 'PROG'
{
  tadir.object   as ObjectType,
  tadir.obj_name as ParentObjectName,
  tadir.obj_name as ObjectName,
  ''             as RawObjectName,
  tadir.author   as CreatedBy,
  tadir.devclass as DevelopmentPackage,
  tadir.object   as AliasType
}
where
  (
        tadir.pgmid   =  'R3TR'
    and tadir.delflag <> 'X'
    and progdir.subc  <> '1'
    and progdir.subc  <> 'I'
  )

union all


//TADIR objects excluding some object types for which special handling is required
select distinct from tadir
  inner join         euobjedit on tadir.object = euobjedit.tadir
{
  euobjedit.tadir as ObjectType,
  ''              as ParentObjectName,
  tadir.obj_name  as ObjectName,
  ''              as RawObjectName,
  tadir.author    as CreatedBy,
  tadir.devclass  as DevelopmentPackage,
  euobjedit.tadir as AliasType
}
where
  (
        tadir.pgmid  =  'R3TR'
    and tadir.object <> 'PROG'
    and tadir.object <> 'STOB'
    and tadir.object <> 'DDLS'
    and tadir.object <> 'WDCC'
    and tadir.object <> 'WDCA'
    and tadir.object <> 'TABL'
    and delflag      <> 'X'
    and tadir.object <> 'HOTA'
    and tadir.object <> 'SHI3'
  )

union all

// Structures
select distinct from tadir
  inner join         dd02l on  tadir.obj_name = dd02l.tabname
                           and tadir.object   = 'TABL'
{
  tadir.object   as ObjectType,
  ''             as ParentObjectName,
  tadir.obj_name as ObjectName,
  ''             as RawObjectName,
  tadir.author   as CreatedBy,
  tadir.devclass as DevelopmentPackage,
  'STRU'         as AliasType

}
where
  (
         tadir.pgmid    =  'R3TR'
    and  tadir.delflag  <> 'X'
    and(
         dd02l.tabclass =  'INTTAB'
      or dd02l.tabclass =  'APPEND'
    )
  )


union all

// Database Tables
select distinct from tadir
  inner join         dd02l on  tadir.obj_name = dd02l.tabname
                           and tadir.object   = 'TABL'
{
  tadir.object   as ObjectType,
  ''             as ParentObjectName,
  tadir.obj_name as ObjectName,
  ''             as RawObjectName,
  tadir.author   as CreatedBy,
  tadir.devclass as DevelopmentPackage,
  'DTAB'         as AliasType

}
where
  (
        tadir.pgmid    =  'R3TR'
    and tadir.delflag  <> 'X'
    and dd02l.tabclass <> ''
    and dd02l.tabclass <> 'INTTAB'
    and dd02l.tabclass <> 'APPEND'
  )


union all

// Web Dynpro Component/Application Configurations
select from tadir
{
  object                       as ObjectType,
  ''                           as ParentObjectName,
  substring( obj_name, 1, 30 ) as ObjectName,
  ''                           as RawObjectName,
  author                       as CreatedBy,
  devclass                     as DevelopmentPackage,
  object                       as AliasType
}
where
  (
         pgmid   =  'R3TR'
    and(
         object  =  'WDCC'
      or object  =  'WDCA'
    )
    and  delflag <> 'X'
  )

union all

// Function moduls
select from  tadir
  inner join ris_prog_tadir on  tadir.object   = ris_prog_tadir.object_type
                            and tadir.obj_name = ris_prog_tadir.object_name
{
  tadir.object             as ObjectType,
  tadir.obj_name           as ParentObjectName,
  ris_prog_tadir.func_name as ObjectName,
  ''                       as RawObjectName,
  tadir.author             as CreatedBy,
  tadir.devclass           as DevelopmentPackage,
  'FUNC'                   as AliasType
}
where
  (
         ris_prog_tadir.func_name <> ''
  )
  and(
         tadir.pgmid              =  'R3TR'
    and  tadir.delflag            <> 'X'
    and(
         tadir.object             =  'FUGR'
      or tadir.object             =  'FUGS'
    )
  )

union all

// Function group includes
select from  tadir
  inner join ris_prog_tadir on  tadir.object   = ris_prog_tadir.object_type
                            and tadir.obj_name = ris_prog_tadir.object_name
{
  tadir.object                as ObjectType,
  tadir.obj_name              as ParentObjectName,
  ris_prog_tadir.program_name as ObjectName,
  ''                          as RawObjectName,
  tadir.author                as CreatedBy,
  tadir.devclass              as DevelopmentPackage,
  'PROG'                      as AliasType
}
where
       tadir.pgmid   =  'R3TR'
  and(
       tadir.object  =  'FUGR'
    or tadir.object  =  'FUGS'
  )
  and  tadir.delflag <> 'X'

union all

// BAdIs
select from  badi_spot
  inner join tadir on  tadir.object   = 'ENHS'
                   and tadir.obj_name = badi_spot.enhspotname
{
  tadir.object        as ObjectType,
  tadir.obj_name      as ParentObjectName,
  badi_spot.badi_name as ObjectName,
  ''                  as RawObjectName,
  tadir.author        as CreatedBy,
  tadir.devclass      as DevelopmentPackage,
  tadir.object        as AliasType
}
where
      tadir.pgmid   =  'R3TR'
  and tadir.delflag <> 'X'

union all

// Packages starting with $
select from tdevc
{
  'DEVC'         as ObjectType,
  ''             as ParentObjectName,
  tdevc.devclass as ObjectName,
  ''             as RawObjectName,
  tdevc.as4user  as CreatedBy,
  tdevc.devclass as DevelopmentPackage,
  'DEVC'         as AliasType
}
where
  tdevc.devclass like '$%'

union all

// SHI3: exclude old v1 entries
select from       tadir
  left outer join ttree on tadir.obj_name = ttree.id
{
  tadir.object   as ObjectType,
  ''             as ParentObjectName,
  tadir.obj_name as ObjectName,
  ''             as RawObjectName,
  tadir.author   as CreatedBy,
  tadir.devclass as DevelopmentPackage,
  tadir.object   as AliasType
}
where
      tadir.pgmid   =  'R3TR'
  and tadir.object  =  'SHI3'
  and tadir.delflag <> 'X'
  and ttree.type    =  'BMENU'
