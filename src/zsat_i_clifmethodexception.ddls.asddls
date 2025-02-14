@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICLIFMEEXC'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Exception of Method'

define view ZSAT_I_ClIfMethodException
  as select from seosubco

{
  key clsname as Classname,
  key cmpname as MethodName,
  key sconame as ExceptionName
}

where scotype = '1'
