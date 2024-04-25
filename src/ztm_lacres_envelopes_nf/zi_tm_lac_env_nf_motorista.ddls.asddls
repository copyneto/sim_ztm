@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Motorista'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_LAC_ENV_NF_MOTORISTA
  as select from /scmtms/d_torite as _torite
{
  key _torite.parent_key,
      _torite.item_descr
}
where
      _torite.item_cat   = 'DRI'
  and _torite.res_id     is not initial
  and _torite.item_descr is not initial
