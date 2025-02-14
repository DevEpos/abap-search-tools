@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZSATICDSANV'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Possible Values for a CDS Annotation'

// Tables DDLA_RT_HEADER, DDLA_RT_ENUMS are available starting with NW 7.52

define view ZSAT_I_CdsAnnotationValue
  // Value 'true' of boolean type annotations
  as select from ddla_rt_header

{
  key ddlaname                         as DdlAnnotationName,
  key key_upper                        as AnnotationNameUpper,
  key key_raw                          as AnnotationNameRaw,

      cast('true' as ddannotation_val) as Value
}

where value_type = 'Boolean'

// Value 'false' of boolean type annotations
union
  select from ddla_rt_header

{
  key ddlaname                          as DdlAnnotationName,
  key key_upper                         as AnnotationNameUpper,
  key key_raw                           as AnnotationNameRaw,

      cast('false' as ddannotation_val) as Value
}

where value_type = 'Boolean'

// Enum Values of annotations
union
  select from  ddla_rt_enums  as Enum

    inner join ddla_rt_header as Header on Header.key_guid = Enum.key_guid

{
  key Header.ddlaname          as DdlAnnotationName,
  key key_upper                as AnnotationNameUpper,
  key key_raw                  as AnnotationNameRaw,

      concat('#', Enum.symbol) as Value
}
