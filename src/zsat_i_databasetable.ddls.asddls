@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database Table'

define view entity ZSAT_I_DatabaseTable
  as select distinct from tadir as Repo
    inner join            dd02l as DbTable      on DbTable.tabname = Repo.obj_name
    left outer join       dd02t as Text         on  Text.tabname    = DbTable.tabname
                                                and Text.ddlanguage = $session.system_language
    left outer join       dd02t as FallBackText on  FallBackText.tabname    = DbTable.tabname
                                                and FallBackText.ddlanguage = Repo.masterlang
{
  key DbTable.tabname          as TableName,
      DbTable.contflag         as DeliveryClass,
      $session.system_language as Language,
      case
        when Text.ddtext is not null then Text.ddtext
        else FallBackText.ddtext
      end                      as Description,
      case
        when Text.ddtext is not null then upper(Text.ddtext)
        else upper(FallBackText.ddtext)
      end                      as DescriptionUpper,
      Repo.author              as CreatedBy,
      Repo.created_on          as CreatedDate,
      DbTable.as4date          as ChangedDate,
      DbTable.as4user          as ChangedBy,
      Repo.devclass            as DevelopmentPackage,
      'T'                      as Type
}
where
      DbTable.tabclass = 'TRANSP'
  and DbTable.as4local = 'A'
  and Repo.pgmid       = 'R3TR'
  and Repo.object      = 'TABL'
