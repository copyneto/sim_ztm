@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Placa Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_TM_ORDEM_CARREGAMENTO_PLACA
  with parameters
    p_mtr : /scmtms/transmeanstypecode,        
    p_res_seq: /scmtms/res_seq      
    
 as 
    select from /scmtms/d_torrot as torrot 
    
    inner join  /scmtms/d_torite as torite on torrot.db_key = torite.parent_key
{
    key torrot.tor_id as Orp_Id,    
    torite.platenumber       
}
where torite.mtr = $parameters.p_mtr
and   torite.res_seq = $parameters.p_res_seq
group by
    torrot.tor_id,
    torite.platenumber
