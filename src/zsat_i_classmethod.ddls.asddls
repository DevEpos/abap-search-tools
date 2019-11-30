@AbapCatalog.sqlViewName: 'ZSATICLSMETH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Method in Class/Interface'
define view ZSAT_I_ClassMethod
  as select from seocompo
{
  key clsname as ClassName,
  key cmpname as MethodName,
      mtdtype as MethodType
}
where
  cmptype = '1'
