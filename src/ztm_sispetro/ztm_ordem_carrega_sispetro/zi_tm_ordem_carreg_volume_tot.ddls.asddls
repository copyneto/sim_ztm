@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Volume Total'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREG_VOLUME_TOT
  as select from /scmtms/d_torite
{
  key parent_key,
      gro_vol_uni,
      @Semantics.quantity.unitOfMeasure: 'gro_vol_uni'
      sum(gro_vol_val) as Volumetotal

}
where
      item_cat       = 'CT'
  and zz_fu_max_util = 100

group by
  parent_key,
  gro_vol_uni
