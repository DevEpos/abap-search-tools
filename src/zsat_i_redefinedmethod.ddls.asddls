@AbapCatalog.sqlViewName: 'ZSATIREDEFMETH'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Redefined methods for each inheritance relationship'

define view ZSAT_I_RedefinedMethod
  as select from seoredef
{

  key clsname    as ClassName,
  key refclsname as ParentClass,
  key mtdname    as MethodName,
      mtdabstrct as IsAbstract,
      mtdfinal   as IsFinal
}
where
  version = '1'
