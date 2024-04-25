@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Placa Cavalo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_LAC_ENV_NF_PLACA_CAVALO
  as select from /scmtms/d_torite as _torite
{
  key parent_key,
      platenumber
} where _torite.mtr = 'ZMTR-CAV'
    and _torite.item_cat = 'AVR'
    and _torite.platenumber is not initial
