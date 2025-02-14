@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATISUPERCLS'

@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Super class releationship'

define view ZSAT_I_SuperClass
  as select from seometarel

{
      // seometarel
  key clsname    as ClassName,
  key refclsname as SuperClass
}

where version = '1'
  and reltype = '2' // inheritance
