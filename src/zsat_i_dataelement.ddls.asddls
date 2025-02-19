@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDTEL'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'DDIC Data Element'

@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_DataElement
  as select from dd04l as Dtel

    inner join   tadir as repo
      on  Dtel.rollname = repo.obj_name
      and repo.object   = 'DTEL'

{
  key Dtel.rollname   as Rollname,

      Dtel.domname    as Domname,
      Dtel.memoryid   as SetGetParamID,
      Dtel.as4user    as ChangedBy,
      Dtel.as4date    as ChangedDate,
      Dtel.as4time    as ChangedTime,
      repo.author     as CreatedBy,
      repo.created_on as CreatedDate,
      repo.devclass   as DevelopmentPackage,
      Dtel.dtelmaster as OriginalLanguage,
      Dtel.shlpname   as ShlpName,
      Dtel.shlpfield  as ShlpField,
      Dtel.deffdname  as DefaultCompName,
      Dtel.datatype   as DataType,
      Dtel.leng       as Length,
      Dtel.decimals   as Decimals,
      Dtel.refkind    as TypeCategory,
      Dtel.reftype    as RefType,
      -- Flags
      Dtel.logflag    as IsChangeDocEnabled,
      Dtel.ltrflddis  as IsBasicWriteDirLtr,

      case
        when Dtel.bidictrlc = 'X' then ''
        else 'X'
      end             as IsBidiFilterEnabled,

      case
        when Dtel.nohistory = 'X' then ''
        else 'X'
      end             as IsInputHistoryEnabled,

      -- Not searchable via SE84
      Dtel.outputlen  as OutputLength,
      Dtel.valexi     as HasFixValues,
      Dtel.lowercase  as IsLowercase,
      Dtel.entitytab  as ValueTable,
      Dtel.convexit   as ConvExit
}

where Dtel.as4local = 'A'
  and repo.pgmid    = 'R3TR'
