@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Global Friend in ABAP OO Class'

define view entity ZSAT_I_GlobalFriend
  as select from seofriends
{
  key clsname    as ClassName,
  key refclsname as Friend
}
where
      friendtype = '0'
  and version    = '1'
