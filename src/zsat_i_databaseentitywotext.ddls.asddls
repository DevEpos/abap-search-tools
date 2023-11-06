@AbapCatalog.sqlViewName: 'ZSATDBENTWOT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database entity without text'

define view ZSAT_I_DatabaseEntityWoText
  as select from tadir as Repo
    inner join   dd02l as Table on  Repo.obj_name  = Table.tabname
                                and Table.as4local = 'A'
{
  Table.tabname as Entity,
  'T'           as Type
}
where
      Repo.object    = 'TABL'
  and Table.tabclass = 'TRANSP'
union select from tadir as Repo
{
  obj_name as Entity,
  case
    when object = 'STOB' then 'C'
    else 'V'
  end      as Type
}
where
      Repo.object  = 'STOB'
  or  Repo.object  = 'VIEW'
  and Repo.genflag = ''
