@AbapCatalog.sqlViewName: 'ZSATICIMPLM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Implemented Method from Interface'

define view ZSAT_I_ClassImplementedMethod
  as select from    seometarel as Meta
    inner join      seocompo   as Method         on  Meta.refclsname = Method.clsname
                                                 and Meta.reltype    = '1'
                                                 and Method.cmptype  = '1'
    left outer join seoredef   as AbstractMethod on  Meta.clsname              = AbstractMethod.clsname
                                                 and Meta.refclsname           = AbstractMethod.refclsname
                                                 and Method.cmpname            = AbstractMethod.mtdname
                                                 and AbstractMethod.mtdabstrct = 'X'
{
  key Meta.clsname                                         as ClassName,
  key concat( Method.clsname, concat('~', Method.cmpname)) as MethodName
}
where
  AbstractMethod.mtdname is null
