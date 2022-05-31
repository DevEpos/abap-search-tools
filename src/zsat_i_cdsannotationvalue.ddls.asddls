@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Possible Values for a CDS Annotation'
@Metadata.ignorePropagatedAnnotations: true

define view entity ZSAT_I_CdsAnnotationValue
  as select from ddla_rt_header // >= 7.51
{
  key ddlaname                           as DdlAnnotationName,
  key key_upper                          as AnnotationNameUpper,
  key key_raw                            as AnnotationNameRaw,
      cast( 'true' as ddannotation_val ) as Value
}
where
  value_type = 'Boolean'
union select from ddla_rt_header // >= 7.51
{
  key ddlaname                            as DdlAnnotationName,
  key key_upper                           as AnnotationNameUpper,
  key key_raw                             as AnnotationNameRaw,
      cast( 'false' as ddannotation_val ) as Value
}
where
  value_type = 'Boolean'
