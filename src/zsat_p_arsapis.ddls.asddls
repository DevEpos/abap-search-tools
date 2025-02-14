@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATPARSAPIS'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'ADT API Filter'

define view ZSAT_P_ArsApis
  as select from cls_assignment as cls

{
  cls.trobjtype as ObjectType,
  cls.sobj_name as ObjectName,
  cls.attribute as Attribute,
  cls.value     as Value
}

where cls.attribute = 'RELEASE_GROUP'
   or cls.attribute = 'ARS_RELEASE_STATE_C0'
   or cls.attribute = 'RELEASE_STATE'
   or cls.attribute = 'ARS_RELEASE_STATE_C2'
