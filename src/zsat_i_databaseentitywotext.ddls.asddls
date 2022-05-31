@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database entity without text'
@Metadata.ignorePropagatedAnnotations: true

define view entity ZSAT_I_DatabaseEntityWoText
  as select from tadir as Repo
    inner join   dd02l as Table on Repo.obj_name = Table.tabname
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
