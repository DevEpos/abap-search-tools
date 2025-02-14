@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATCLIFCOMPT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Text for Class/Interface Method'

define view ZSAT_I_ClassInterfaceCompText
  as select from seocompotx

{
  key clsname         as ClassName,
  key cmpname         as Component,
  key langu           as Language,

      descript        as Description,
      upper(descript) as DescriptionUpper
}
