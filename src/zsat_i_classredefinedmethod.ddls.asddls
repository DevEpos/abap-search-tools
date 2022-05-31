@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Redefined method from intf. /super class'

define view entity ZSAT_I_ClassRedefinedMethod
  as select from seometarel as Meta
    inner join   seoredef   as RedefinedMethod on  Meta.clsname = RedefinedMethod.clsname
                                               and Meta.reltype = '2'
{
  key Meta.clsname            as ClassName,
  key RedefinedMethod.mtdname as MethodName
}
