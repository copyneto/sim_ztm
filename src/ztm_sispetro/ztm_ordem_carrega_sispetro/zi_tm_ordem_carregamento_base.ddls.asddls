@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_BASE as 
    select from     /scmtms/d_torrot as torrot 
    
    inner join      ZI_TM_SISPETRO as sispetro on torrot.db_key = sispetro.db_key 
    
    inner join      /scmtms/d_torite as torite on torrot.db_key = torite.parent_key
                                              and torite.item_type = 'PRD'
                                              and torite.parent_key = sispetro.db_key          

    inner join      /sapapo/loc as loc on torite.src_loc_idtrq = loc.locno
    
    inner join      /sapapo/loct as locText on loc.locid = locText.locid
                                           and locText.spras = $session.system_language  
{
    key torrot.tor_id as Orp_Id,  
    locText.descr40 as BaseText
}
group by
    torrot.tor_id,
    locText.descr40
