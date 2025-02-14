@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICLIFMEPAR'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Parameter of Method'

define view ZSAT_I_ClIfMethodParam
  as select from seosubco

{
  key clsname as Classname,
  key cmpname as MethodName,
  key sconame as ParameterName
}

where scotype = '0'
