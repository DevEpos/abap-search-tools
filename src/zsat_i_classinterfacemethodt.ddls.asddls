@AbapCatalog.sqlViewName: 'ZSATICLIFMETHT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Text for Class/Interface Method'

define view ZSAT_I_ClassInterfaceMethodT
  as select from seocompotx
{
  key clsname  as ClassInterfaceName,
  key cmpname  as Component,
  key langu    as Language,
      descript as Description
}
