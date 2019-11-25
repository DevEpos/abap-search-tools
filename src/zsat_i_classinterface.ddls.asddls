@AbapCatalog.sqlViewName: 'ZSATICLASSINTF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Properties of Class/Interface'

define view ZSAT_I_ClassInterface
  as select from seoclassdf as ClassIntf
    inner join   tadir      as Repo on  ClassIntf.clsname = Repo.obj_name
                                    and Repo.pgmid        = 'R3TR'
                                    and (
                                       Repo.object        = 'CLAS'
                                       or Repo.object     = 'INTF'
                                     )
{
  key ClassIntf.clsname         as ClassInterface,
      Repo.devclass             as DevelopmentPackage,
      Repo.object               as TadirType,
      ClassIntf.category        as Category,
      ClassIntf.exposure        as Exposure,
      ClassIntf.state           as State,
      ClassIntf.author          as CreatedBy,
      ClassIntf.createdon       as CreatedOn,
      ClassIntf.changedby       as ChangedBy,
      ClassIntf.changedon       as ChangedOn,
      ClassIntf.clsabstrct      as IsAbstract,
      ClassIntf.clsfinal        as IsFinal,
      ClassIntf.refclsname      as ReferencedObject,
      ClassIntf.fixpt           as HasFixPointArithmetic,
      ClassIntf.unicode         as ABAPVersion,
      ClassIntf.rstat           as ProgramStatus,
      ClassIntf.r3release       as ReleaseNumber,
      ClassIntf.msg_id          as MessageClass,
      ClassIntf.clsproxy        as ProxyClassCategory,
      ClassIntf.clssharedmemory as IsSharedMemoryEnabled,
      ClassIntf.with_unit_tests as HasUnitTests,
      ClassIntf.duration_type   as DurationType,
      ClassIntf.risk_level      as RiskLevel,
      ClassIntf.within_package  as IsOpenedForPackage
}
where
  version = '1' // only active classes
