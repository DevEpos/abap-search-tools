@AbapCatalog.sqlViewName: 'ZSATIMESSAGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Message'

define view ZSAT_I_Message
  as select from t100  as Message
    inner join   t100u as LastMsgChange on  Message.arbgb = LastMsgChange.arbgb
                                        and Message.msgnr = LastMsgChange.msgnr
{
  key sprsl                 as Language,
  key Message.arbgb         as MessageClass,
  key Message.msgnr         as MessageNumber,
      LastMsgChange.name    as ChangedBy,
      LastMsgChange.datum   as ChangedOn,
      LastMsgChange.selfdef as IsSelfExplanatory,
      text                  as ShortText,
      upper(text)           as ShortTextUpper
}
