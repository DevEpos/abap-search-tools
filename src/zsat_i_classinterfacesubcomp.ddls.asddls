@AbapCatalog.sqlViewName: 'ZSATICLIFSUCO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sub Component of Class/Interface'

define view ZSAT_I_ClassInterfaceSubComp
  as select from seosubcodf
{
  key clsname      as Classname,
  key cmpname      as Component,
  key sconame      as SubComponentName,
  key version      as Version,
      author       as Author,
      createdon    as CreatedOn,
      changedby    as ChangedBy,
      changedon    as ChangedOn,
      pardecltyp   as ParDeclTyp,
      parpasstyp   as ParPassTyp,
      typtype      as TypType,
      type         as Type,
      parvalue     as ParValue,
      paroptionl   as ParOptionl,
      parpreferd   as ParPreferd,
      excdecltyp   as ExcDeclTyp,
      is_resumable as IsResumable
}
where
  version = '1'
