@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Incoterms Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_INC as 
    select from /scmtms/d_torrot as torrot 
    
    inner join      ZI_TM_SISPETRO as sispetro on torrot.db_key = sispetro.db_key 
    
    inner join      /scmtms/d_torite as torite on torrot.db_key = torite.parent_key
                                              and torite.item_cat = 'AVR'
                                              and torite.parent_key = sispetro.db_key    
{
    key torrot.tor_id as Orp_Id,  
    torite.inc_class_code as Incoterms    
}
group by
    torrot.tor_id,
    torite.inc_class_code
