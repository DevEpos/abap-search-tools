@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Method in Class/Interface'
@Metadata.ignorePropagatedAnnotations: true

define view entity ZSAT_I_ClassMethod
  // Normal methods in interfaces and classes
  as select from seocompo
{
  key clsname                       as ClassName,
  key cast( cmpname as seocpdname ) as MethodName,
      mtdtype                       as MethodType
}
where
  cmptype = '1'
// Implemented non abstract methods from an interface
union all select from ZSAT_I_ClassImplementedMethod
{
  key ClassName,
  key MethodName,
      '0' as MethodType
}
// Redefined methods in classes
union all select from ZSAT_I_ClassRedefinedMethod
{
  key ClassName,
  key MethodName,
      '0' as MethodType
}
