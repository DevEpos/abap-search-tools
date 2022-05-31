@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Text for Class/Interface'

define view entity ZSAT_I_ClassInterfaceT
  as select from seoclasstx
{
  key clsname         as ClassName,
  key langu           as Language,
      descript        as Description,
      upper(descript) as DescriptionUpper
}
