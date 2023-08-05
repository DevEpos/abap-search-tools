@AbapCatalog.sqlViewName: 'ZSATIREPOSRC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Repository Sources'

define view ZSAT_I_RepoSource
  as select from reposrc
{
  key progname                                     as Progname,
      replace(substring(progname, 1, 30), '=', '') as ObjectName,
      substring(progname, 31, 1)                   as MainType,
      substring(progname, 32, 9)                   as IncludeKind,
      subc                                         as Subc,
      cnam                                         as CreatedBy,
      cdat                                         as CreatedOn,
      unam                                         as ChangedBy,
      udat                                         as ChangedOn
}
where
      datalg  > 0
  and r3state = 'A'
