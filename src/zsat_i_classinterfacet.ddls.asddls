@AbapCatalog.sqlViewName: 'ZSATICLSINTFT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Text for Class/Interface'

define view ZSAT_I_ClassInterfaceT
  as select from seoclasstx
{
  key clsname         as ClassName,
  key langu           as Language,
      descript        as Description,
      upper(descript) as DescriptionUpper
}
