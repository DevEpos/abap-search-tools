@AbapCatalog.sqlViewName: 'ZSATPCDS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base view for CDS'

@VDM.private: true

define view ZSAT_P_Cds
  as select from    ddddlsrc             as Source
    left outer join ZSAT_I_DdlDependency as StructuredObject on Source.ddlname = StructuredObject.DdlName
    left outer join dd02b                as CdsEntityHeader  on  StructuredObject.EntityName = CdsEntityHeader.strucobjn
                                                             and CdsEntityHeader.as4local    = 'A'
{
  key Source.ddlname,
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
      Source.as4date          as ChangedDate,
      Source.as4user          as ChangedBy
}
where
  Source.as4local = 'A'
