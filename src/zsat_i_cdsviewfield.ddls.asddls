@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Field for CDS'

define view entity ZSAT_I_CdsViewField
  as select from dd03nd as Field
{
  key  Field.strucobjn as EntityId,
  key  Field.fieldname as FieldName,
       Field.position  as FieldPosition,
       Field.keyflag   as IsKeyField,
       case
         when Field.fieldname_raw = '' then Field.fieldname
         else Field.fieldname_raw
       end             as RawFieldName,
       Field.domname   as DomainName,
       Field.rollname  as RollName

}
where
  as4local = 'A'
