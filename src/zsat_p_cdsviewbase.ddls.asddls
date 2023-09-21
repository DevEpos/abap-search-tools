@AbapCatalog.sqlViewName: 'ZSATPCDSVB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Base view for CDS with Repository info.'

@VDM.private: true

define view ZSAT_P_CDSViewBase
  as select from ZSAT_P_Cds as CdsBase
    inner join   tadir      as Repo on  CdsBase.ddlname = Repo.obj_name
                                    and Repo.pgmid      = 'R3TR'
                                    and Repo.object     = 'DDLS'
{
  key CdsBase.ddlname,
      CdsBase.EntityId,
      CdsBase.RawEntityId,
      CdsBase.ViewName,
      CdsBase.ParentDdlName,
      CdsBase.SourceType,
      CdsBase.DdlSource,
      Repo.devclass   as DevelopmentPackage,
      Repo.author     as CreatedBy,
      Repo.created_on as CreatedDate,
      Repo.masterlang as OriginalLanguage,
      CdsBase.ChangedDate,
      CdsBase.ChangedBy
}
