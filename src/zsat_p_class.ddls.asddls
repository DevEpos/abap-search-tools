@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATPCLASS'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'ABAP OO Class/Interface'

define view ZSAT_P_Class
  as select from seoclass   as Class

    inner join   seoclassdf as ClassDef on Class.clsname = ClassDef.clsname

{
  key ClassDef.clsname         as ClassName,

      case
        when Class.clstype = '0' then 'CLAS'
        else                    'INTF'
      end                      as TadirType,

      ClassDef.category        as Category,
      ClassDef.exposure        as Exposure,
      ClassDef.state           as State,
      ClassDef.author          as CreatedBy,
      ClassDef.createdon       as CreatedOn,

      case
        when ClassDef.changedby <> '' then ClassDef.changedby
        else                               ClassDef.author
      end                      as ChangedBy,

      case
        when ClassDef.changedon <> '00000000' then ClassDef.changedon
        else                               ClassDef.createdon
      end                      as ChangedOn,

      ClassDef.clsabstrct      as IsAbstract,
      ClassDef.clsfinal        as IsFinal,
      ClassDef.refclsname      as ReferencedObject,
      ClassDef.fixpt           as HasFixPointArithmetic,
      ClassDef.unicode         as ABAPVersion,
      ClassDef.rstat           as ProgramStatus,
      ClassDef.r3release       as ReleaseNumber,
      ClassDef.msg_id          as MessageClass,
      ClassDef.clsproxy        as ProxyClassCategory,
      ClassDef.clssharedmemory as IsSharedMemoryEnabled,
      ClassDef.with_unit_tests as HasUnitTests,
      ClassDef.duration_type   as DurationType,
      ClassDef.risk_level      as RiskLevel,
      ClassDef.within_package  as IsOpenedForPackage
}

where ClassDef.version = '1' // only active classes
