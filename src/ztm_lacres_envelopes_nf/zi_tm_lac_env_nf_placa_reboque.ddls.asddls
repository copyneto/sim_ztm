@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Placa Reboque'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_LAC_ENV_NF_PLACA_REBOQUE
  as select from /scmtms/d_torite as _torite
{
  key parent_key,
      platenumber
} where ( _torite.mtr = 'ZMTR-CAR' or _torite.mtr = 'ZMTR-CARR' )
    and _torite.item_cat = 'PVR'
    and _torite.platenumber is not initial
