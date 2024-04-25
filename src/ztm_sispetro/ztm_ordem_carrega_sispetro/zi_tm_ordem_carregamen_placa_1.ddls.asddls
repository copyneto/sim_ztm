@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Placa 1 Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMEN_PLACA_1 as 
    select from /scmtms/d_torrot    as torrot 
    
    inner join  /scmtms/d_torite as torite on torrot.db_key = torite.parent_key 
{   
    key torrot.tor_id as Orp_Id,    
    torite.platenumber
}
where ( torite.mtr = 'ZMTR-CAV' or torite.mtr = 'ZMTR-TRK' )  
and torite.item_type = 'TRUC'
and torite.item_cat = 'AVR'
and ( torite.res_seq = 0 or torite.res_seq = 1) 
group by
    torrot.tor_id,
    torite.platenumber
