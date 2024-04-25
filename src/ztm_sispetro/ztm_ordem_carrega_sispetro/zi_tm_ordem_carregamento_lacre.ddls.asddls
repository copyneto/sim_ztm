@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lacre Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_LACRE as 
    select from /scmtms/d_torrot as torrot
    
    inner join  /scmtms/d_torite as torite on torrot.db_key = torite.parent_key 
{
    key torrot.tor_id as Orp_Id,
    sum(torite.zz_seal_number_total) as Total    
}
where torite.item_cat = 'CT'
and torite.zz_fu_max_util = 100.00 
group by torrot.tor_id
