@AbapCatalog.sqlViewName: 'ZSATISIMPLCLIFM'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Class/Interface method - for Class Search'

define view ZSAT_I_SimpleClIfMethod 
  // Normal methods in interfaces and classes
  as select from seocompo 
{
  key clsname                          as ClassName,
  key cast( cmpname as abap.char(61) ) as MethodName,
      mtdtype                          as MethodType
}
where
  cmptype = '1'
  
// Implemented non abstract methods from an interface
union select from    seometarel as Meta
    inner join      seocompo   as Method         on  Meta.refclsname = Method.clsname
                                                 and Meta.reltype    = '1'
                                                 and Method.cmptype  = '1'
    left outer join seoredef   as AbstractMethod on  Meta.clsname              = AbstractMethod.clsname
                                                 and Meta.refclsname           = AbstractMethod.refclsname
                                                 and Method.cmpname            = AbstractMethod.mtdname
                                                 and AbstractMethod.mtdabstrct = 'X'
{
  key Meta.clsname                                         as ClassName,
  key concat( Method.clsname, concat('~', Method.cmpname)) as MethodName,
      '0'                                                  as MethodType
}
where
  AbstractMethod.mtdname is null
  
// Redefined methods in classes  
union select from seometarel as Meta
    inner join   seoredef   as RedefinedMethod on  Meta.clsname = RedefinedMethod.clsname
                                               and Meta.reltype = '2'
{
  key Meta.clsname            as ClassName,
  key RedefinedMethod.mtdname as MethodName,
      '0'                     as MethodType
}
