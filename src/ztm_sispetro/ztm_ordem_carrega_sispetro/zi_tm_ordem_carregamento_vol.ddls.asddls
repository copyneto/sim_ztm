@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Volume Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_VOL as 
    select from /scmtms/d_torrot as torrot    
    
    inner join  /scmtms/d_torite as torite on torrot.db_key = torite.parent_key
                                              and torite.item_cat = 'AVR'                                                  
{
    key torrot.tor_id as Orp_Id,  
    torite.gro_wei_uni as UnidadeVolume,
    @Semantics.quantity.unitOfMeasure: 'UnidadeVolume' 
    sum(torite.gro_vol_val) as Volume    
}
group by
    torrot.tor_id,
    torite.gro_wei_uni
