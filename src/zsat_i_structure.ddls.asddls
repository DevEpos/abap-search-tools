@AbapCatalog.sqlViewName: 'ZSATISTRUCT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DDIC Structure'

define view ZSAT_I_Structure
  as select distinct from tadir as Repo
    inner join            dd02l as Struct       on Struct.tabname = Repo.obj_name
    left outer join       dd02t as Text         on  Text.tabname    = Struct.tabname
                                                and Text.ddlanguage = $session.system_language
                                                and Text.as4local   = 'A'
    left outer join       dd02t as FallBackText on  FallBackText.tabname    = Struct.tabname
                                                and FallBackText.ddlanguage = Repo.masterlang
                                                and FallBackText.as4local   = 'A'
{
  key Struct.tabname           as Structure,
      Struct.exclass           as ExtensionClass,
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
      Struct.as4date           as ChangedDate,
      Struct.as4user           as ChangedBy,
      Repo.devclass            as DevelopmentPackage
}
where
  (
       Struct.tabclass = 'APPEND'
    or Struct.tabclass = 'INTTAB'
  )
  and  Struct.as4local = 'A'
  and  Repo.pgmid      = 'R3TR'
  and  Repo.object     = 'TABL'
