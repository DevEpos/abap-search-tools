@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIGLBLFRND'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Global Friend in ABAP OO Class'

define view ZSAT_I_GlobalFriend
  as select from seofriends

{
  key clsname    as ClassName,
  key refclsname as Friend
}

where friendtype = '0'
  and version    = '1'
