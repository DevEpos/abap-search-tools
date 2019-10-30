@AbapCatalog.sqlViewName: 'ZSATIRISQUST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Object Texts for Quick search'

define view ZSAT_I_RisQuickSearchText
  -- Select Texts for Classes, Interfaces
  as select distinct from seoclass   as Class
    inner join            seoclasstx as ClassText on Class.clsname = ClassText.clsname
{
  key Class.clsname   as ObjectName,
  key case
        when Class.clstype = #SEOCLSTYPE.0 then 'CLAS' else 'INTF'
      end             as ObjectType,
      upper(descript) as DescriptionUpper,
      descript        as Description
}
where
  langu = $session.system_language
-- Select Texts for Tables (Structure, Database Tables)
union all select distinct from dd02t as Text
{
  key Text.tabname       as ObjectName,
  key 'TABL'             as ObjectType,
      upper(Text.ddtext) as DescriptionUpper,
      Text.ddtext        as Description
}
where
  Text.ddlanguage = $session.system_language
-- Select Texts for Data Elements
union all select distinct from dd04t as Text
{
  key Text.rollname      as ObjectName,
  key 'DTEL'             as ObjectType,
      upper(Text.ddtext) as DescriptionUpper,
      Text.ddtext        as Description
}
where
  Text.ddlanguage = $session.system_language
-- Select Texts for Domains
union all select distinct from dd01t as Text
{
  key Text.domname       as ObjectName,
  key 'DOMA'             as ObjectType,
      upper(Text.ddtext) as DescriptionUpper,
      Text.ddtext        as Description
}
where
  Text.ddlanguage = $session.system_language
-- Select Texts for Authorization Objects
union all select distinct from tobjt as Text
{
  key Text.object       as ObjectName,
  key 'SUSO'            as ObjectType,
      upper(Text.ttext) as DescriptionUpper,
      Text.ttext        as Description
}
where
  Text.langu = $session.system_language
