@AbapCatalog.sqlViewName: 'ZSATIMSGCLAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Message Classes'

define view ZSAT_I_MessageClass
  as select from t100a as MessageClass
    inner join   tadir as Repo on  MessageClass.arbgb = Repo.obj_name
                               and Repo.object        = 'MSAG'
                               and Repo.pgmid         = 'R3TR'
{
  key arbgb                   as MessageClass,
      Repo.devclass           as DevelopmentPackage,
      MessageClass.masterlang as MasterLangauge,
      respuser                as CreatedBy,
      lastuser                as ChangedBy,
      ldate                   as ChangedOn,
      stext                   as Description,
      upper(stext)            as DescriptionUpper
}
