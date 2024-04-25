@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Itens da Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_ITEM as select from ZI_TM_ORDEM_CARREGAMENTO_COMP
{
    key db_key,
    key parent_key,
    key Orp_Id,
    zz_fu_db_key,
    ct_seq,
    zz_fu_max_util,
    item_descr,
    gro_vol_uni,
    @Semantics.quantity.unitOfMeasure: 'gro_vol_uni'
    gro_vol_val,
    res_id,
    envelope
}
