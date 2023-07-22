@AbapCatalog.sqlViewName: 'ZSATICLIFMETH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Methods in Classes / Interfaces'

define view ZSAT_I_ClassInterfaceMethod
  // Normal methods in interfaces and classes
  as select from seocompo   as Method
    inner join   seocompodf as MethodDef on  Method.clsname = MethodDef.clsname
                                         and Method.cmpname = MethodDef.cmpname
{
  key Method.clsname                          as ClassName,
  key cast( Method.cmpname as abap.char(61) ) as MethodName,
      //      cast('' as seoclsname)               as ParentClass,
      mtdtype                                 as MethodType,
      MethodDef.alias                         as IsAliasComponent,
      MethodDef.mtdabstrct                    as IsAbstract,
      case when MethodDef.mtdoptnl = 'I' then 'X'
                                         else ''
      end                                     as IsOptional,
      MethodDef.mtdfinal                      as IsFinal,
      MethodDef.exposure                      as Exposure,
      MethodDef.mtdnewexc                     as IsUsingNewExceptions,
      MethodDef.mtddecltyp                    as MethodLevel,
      Method.clsname                          as OriginalClifName,
      cast( Method.cmpname as abap.char(61) ) as OriginalMethodName,
      MethodDef.author                        as CreatedBy,
      MethodDef.createdon                     as CreatedOn,
      MethodDef.changedby                     as ChangedBy,
      MethodDef.changedon                     as ChangedOn,
      // Normal method
      cast( '1' as abap.char(3) )             as Category
}
where
      version = '1' // Active
  and cmptype = '1'

// Implemented non abstract methods from an interface
union select from ZSAT_I_ClassImplementedMethod
{
  key ClassName,
  key MethodName,
      //      ClassName           as ParentClass,
      '0'                         as MethodType,
      IsAliasComponent,
      ''                          as IsAbstract,
      IsOptional,
      IsFinal,
      '2'                         as Exposure,
      IsUsingNewExceptions,
      MethodLevel,
      InterfaceName               as OriginalClifName,
      InterfaceMethodName         as OriginalMethodName,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn,
      // Implemented interface method
      cast( '2' as abap.char(3) ) as Category
}
// Redefined methods in classes
union select from ZSAT_I_RedefinedMethod
{
  key ClassName,
  key MethodName,
      //      ParentClass,
      '0'                         as MethodType,
      IsAliasComponent,
      ''                          as IsAbstract,
      ''                          as IsOptional,
      IsFinal,
      Exposure,
      IsUsingNewExceptions,
      MethodLevel,
      OriginalClifName,
      OriginalMethodName,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn,
      // Redefined class/interface method
      cast( '3' as abap.char(3) ) as Category
}
