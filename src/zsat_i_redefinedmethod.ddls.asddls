@AbapCatalog.sqlViewName: 'ZSATIREDEFMETH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Redefined class/interface method'

define view ZSAT_I_RedefinedMethod
  -- Direct inherited methods that are redefined
  as select from seometarel as Meta
    inner join   seoredef   as RedefinedMethod on  Meta.clsname    = RedefinedMethod.clsname
                                               and Meta.refclsname = RedefinedMethod.refclsname
                                               and Meta.version    = RedefinedMethod.version
                                               and Meta.reltype    = '2'
    inner join   seocompodf as MethodDef       on  RedefinedMethod.refclsname = MethodDef.clsname
                                               and RedefinedMethod.mtdname    = MethodDef.cmpname
                                               and MethodDef.version          = '1'
{
  key Meta.clsname             as ClassName,
  key RedefinedMethod.mtdname  as MethodName,
      Meta.refclsname          as ParentClass,
      RedefinedMethod.mtdfinal as IsFinal,
      MethodDef.alias          as IsAliasComponent,
      MethodDef.exposure       as Exposure,
      MethodDef.mtdnewexc      as IsUsingNewExceptions,
      MethodDef.mtddecltyp     as MethodLevel,
      Meta.clsname             as OriginalClifName,
      RedefinedMethod.mtdname  as OriginalMethodName,
      MethodDef.author         as CreatedBy,
      MethodDef.createdon      as CreatedOn,
      MethodDef.changedby      as ChangedBy,
      MethodDef.changedon      as ChangedOn
}
where
  Meta.version = '1'

-- Interface methods that are redefined
union select from seometarel        as Meta
  inner join      seoredef          as RedefinedMethod on  Meta.clsname    = RedefinedMethod.clsname
                                                       and Meta.refclsname = RedefinedMethod.refclsname
                                                       and Meta.version    = RedefinedMethod.version
                                                       and Meta.reltype    = '2'
  inner join      ZSAT_I_IntfMethod as IntfMethod      on RedefinedMethod.mtdname = IntfMethod.MethodName
{
  key Meta.clsname                   as ClassName,
  key RedefinedMethod.mtdname        as MethodName,
      Meta.refclsname                as ParentClass,
      RedefinedMethod.mtdfinal       as IsFinal,
      IntfMethod.IsAliasComponent,
      IntfMethod.Exposure            as Exposure,
      IntfMethod.IsUsingNewExceptions,
      IntfMethod.MethodLevel,
      IntfMethod.InterfaceName       as OriginalClifName,
      IntfMethod.InterfaceMethodName as OriginalMethodName,
      IntfMethod.CreatedBy,
      IntfMethod.CreatedOn,
      IntfMethod.ChangedBy,
      IntfMethod.ChangedOn
}
where
  Meta.version = '1'
