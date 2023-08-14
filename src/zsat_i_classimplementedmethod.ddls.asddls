@AbapCatalog.sqlViewName: 'ZSATICIMPLM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Implemented Method from Interface'

define view ZSAT_I_ClassImplementedMethod
  as select from    seometarel as Meta
    inner join      seocompo   as Method          on  Meta.refclsname = Method.clsname
                                                  and Meta.reltype    = '1'
                                                  and Method.cmptype  = '1'
    inner join      seocompodf as MethodDef       on  Method.clsname  = MethodDef.clsname
                                                  and Method.cmpname  = MethodDef.cmpname
                                                  and MethodDef.alias = ''
    left outer join seoredef   as RedefinedMethod on  Meta.clsname    = RedefinedMethod.clsname
                                                  and Meta.refclsname = RedefinedMethod.refclsname
                                                  and Method.cmpname  = RedefinedMethod.mtdname
{
  key Meta.clsname                                         as ClassName,
  key concat( Method.clsname, concat('~', Method.cmpname)) as MethodName,
      case when MethodDef.mtdoptnl = 'I' then 'X'
                                         else ''
      end                                                  as IsOptional,
      MethodDef.alias                                      as IsAliasComponent,
      RedefinedMethod.mtdfinal                             as IsFinal,
      MethodDef.mtdnewexc                                  as IsUsingNewExceptions,
      Method.clsname                                       as InterfaceName,
      Method.cmpname                                       as InterfaceMethodName,
      MethodDef.mtddecltyp                                 as MethodLevel,
      MethodDef.author                                     as CreatedBy,
      MethodDef.createdon                                  as CreatedOn,
      MethodDef.changedby                                  as ChangedBy,
      MethodDef.changedon                                  as ChangedOn
}
where
       Meta.version               = '1'
  and  MethodDef.version          = '1'
  and(
       RedefinedMethod.mtdname    is null
    or RedefinedMethod.mtdabstrct = ''
  )
