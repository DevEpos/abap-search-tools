@AbapCatalog.sqlViewName: 'ZSATPCDSVB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base view for CDS with Repository info.'

@VDM.private: true

define view ZSAT_P_CDSViewBase
  as select from    ddddlsrc             as Source
    left outer join ZSAT_I_DdlDependency as StructuredObject on Source.ddlname = StructuredObject.DdlName
    left outer join dd02b                as CdsEntityHeader  on  StructuredObject.EntityName = CdsEntityHeader.strucobjn
                                                             and CdsEntityHeader.as4local    = 'A'
    inner join      tadir                as Repo             on  Source.ddlname = Repo.obj_name
                                                             and Repo.pgmid     = 'R3TR'
                                                             and Repo.object    = 'DDLS'
{
  key Source.DdlName,
      cast(case
        when CdsEntityHeader.strucobjn is null then Source.ddlname
        else CdsEntityHeader.strucobjn
      end as ddstrucobjname ) as EntityId,
      cast(case
        when CdsEntityHeader.strucobjn_raw is null then Source.ddlname
        else CdsEntityHeader.strucobjn_raw
      end as ddstrucobjname ) as RawEntityId,
      cast(case
        when StructuredObject.ViewName is null then Source.ddlname
        else StructuredObject.ViewName
      end as ddstrucobjname)  as ViewName,
      Source.parentname       as ParentDdlName,
      Source.source_type      as SourceType,
      Source.source           as DdlSource,
      Repo.devclass           as DevelopmentPackage,
      Repo.author             as CreatedBy,
      Repo.created_on         as CreatedDate,
      Repo.masterlang         as OriginalLanguage,
      Source.as4date          as ChangedDate,
      Source.as4user          as ChangedBy
}
where
  Source.as4local = 'A'
