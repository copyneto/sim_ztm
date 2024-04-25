@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Envelope de Amostra'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_LAC_ENV_NF_ENVELOPE
  as select from /scmtms/d_torite as _torite
{
  key parent_key,
      zz_sample_envelope
}
where
  zz_sample_envelope is not initial
