@AbapCatalog.sqlViewName: 'ZSATICLASSINTF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'ABAP OO Class/Interface'

define view ZSAT_I_ClassInterface
  as select from ZSAT_P_Class as Class
    inner join   tadir        as Repo on  Class.ClassName = Repo.obj_name
                                      and Class.TadirType = Repo.object
                                      and Repo.pgmid      = 'R3TR'
{
      //ZSAT_P_Class
  key ClassName,
      TadirType,
      Repo.devclass as DevelopmentPackage,
      Category,
      Exposure,
      State,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn,
      IsAbstract,
      IsFinal,
      ReferencedObject,
      HasFixPointArithmetic,
      ABAPVersion,
      ProgramStatus,
      ReleaseNumber,
      MessageClass,
      ProxyClassCategory,
      IsSharedMemoryEnabled,
      HasUnitTests,
      DurationType,
      RiskLevel,
      IsOpenedForPackage
}
