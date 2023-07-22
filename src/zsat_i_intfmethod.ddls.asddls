@AbapCatalog.sqlViewName: 'ZSATIINTFMETH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface Method'

define view ZSAT_I_IntfMethod
  as select from seoclass   as Interface
    inner join   seocompo   as Method    on  Interface.clsname = Method.clsname
                                         and Method.cmptype    = '1'
    inner join   seocompodf as MethodDef on  Interface.clsname = MethodDef.clsname
                                         and Method.cmpname    = MethodDef.cmpname
{
  key Interface.clsname                                         as InterfaceName,
  key MethodDef.cmpname                                         as InterfaceMethodName,
  key concat(Interface.clsname, concat('~', MethodDef.cmpname)) as MethodName,
      MethodDef.alias                                           as IsAliasComponent,
      MethodDef.exposure                                        as Exposure,
      MethodDef.mtddecltyp                                      as MethodLevel,
      MethodDef.author                                          as CreatedBy,
      MethodDef.createdon                                       as CreatedOn,
      MethodDef.changedby                                       as ChangedBy,
      MethodDef.changedon                                       as ChangedOn,
      MethodDef.mtdnewexc                                       as IsUsingNewExceptions,
      MethodDef.mtdoptnl                                        as IsOptional
}
where
      Interface.clstype = '1'
  and MethodDef.version = '1'
