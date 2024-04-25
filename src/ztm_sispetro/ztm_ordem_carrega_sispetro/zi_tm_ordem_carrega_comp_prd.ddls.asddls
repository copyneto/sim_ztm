@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Compartimentos PRD'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGA_COMP_PRD
  as select from /scmtms/d_torite as _ToritePRD
{
  key parent_key,
      item_descr,
      gro_vol_uni,
      @Semantics.quantity.unitOfMeasure: 'gro_vol_uni'
      gro_vol_val
}
where
  item_cat = 'PRD'
