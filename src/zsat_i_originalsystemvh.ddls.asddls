@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIORGSYST'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'VH for Original System'

@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_OriginalSystemVh
  as select distinct from tadir

{
  srcsystem as OriginalSystem
}
