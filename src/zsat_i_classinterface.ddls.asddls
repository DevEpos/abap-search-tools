@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ABAP OO Class/Interface'

define view entity ZSAT_I_ClassInterface
  as select from ZSAT_P_Class as Class
    inner join   tadir        as Repo on  Class.ClassName = Repo.obj_name
                                      and Class.TadirType = Repo.object
                                      and Repo.pgmid      = 'R3TR'
{
      //ZSAT_P_Class
  key Class.ClassName,
      Class.TadirType,
      Repo.devclass as DevelopmentPackage,
      Class.Category,
      Class.Exposure,
      Class.State,
      Class.CreatedBy,
      Class.CreatedOn,
      Class.ChangedBy,
      Class.ChangedOn,
      Class.IsAbstract,
      Class.IsFinal,
      Class.ReferencedObject,
      Class.HasFixPointArithmetic,
      Class.ABAPVersion,
      Class.ProgramStatus,
      Class.ReleaseNumber,
      Class.MessageClass,
      Class.ProxyClassCategory,
      Class.IsSharedMemoryEnabled,
      Class.HasUnitTests,
      Class.DurationType,
      Class.RiskLevel,
      Class.IsOpenedForPackage
}
