@AbapCatalog.sqlViewName: 'ZSATIINDREDMETH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Indirect redefined method'

define view ZSAT_I_IndirectRedefinedMethod
  as select from    seometarel        as Meta
    inner join      seoredef          as RedefinedMethod on  Meta.clsname    = RedefinedMethod.clsname
                                                         and Meta.refclsname = RedefinedMethod.refclsname
                                                         and Meta.version    = RedefinedMethod.version
                                                         and Meta.reltype    = '2'
    left outer join seocompodf        as MethodDef       on  RedefinedMethod.refclsname = MethodDef.clsname
                                                         and RedefinedMethod.mtdname    = MethodDef.cmpname
    left outer join ZSAT_I_IntfMethod as IntfMethod      on RedefinedMethod.mtdname = IntfMethod.MethodName
{
  key Meta.clsname             as ClassName,
  key RedefinedMethod.mtdname  as MethodName,
      Meta.refclsname          as ParentClass,
      RedefinedMethod.mtdfinal as IsFinal
}
where
      Meta.version          = '1'
  and MethodDef.alias       is null
  and IntfMethod.MethodName is null
