@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATIDOMAIN'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'DDIC Domain'

@Metadata.ignorePropagatedAnnotations: true

define view ZSAT_I_Domain
  as select from dd01l as Domain

    inner join   tadir as Repo
      on  Domain.domname = Repo.obj_name
      and Repo.object    = 'DOMA'

{
  key Domain.domname   as DomainName,

      Repo.devclass    as DevelopmentPackage,
      Repo.author      as CreatedBy,
      Repo.created_on  as CreatedDate,
      Domain.as4user   as ChangedBy,
      Domain.as4date   as ChangedDate,
      Domain.datatype  as DataType,
      Domain.leng      as Length,
      Domain.outputlen as OutputLength,
      Domain.decimals  as Decimals,
      Domain.lowercase as IsLowercase,
      Domain.valexi    as HasFixValues,
      Domain.entitytab as ValueTable,
      Domain.convexit  as ConvExit,

      case
        when Domain.appendname <> '' then 'APPEND_DOMAIN'
        else 'DOMAIN'
      end              as Type,

      Domain.appexist  as HasAppend
}

where Domain.as4local = 'A'
