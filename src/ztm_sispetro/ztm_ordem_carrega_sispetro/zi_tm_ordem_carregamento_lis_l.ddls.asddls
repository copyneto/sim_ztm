@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lista de Lacre Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_LIS_L as 
    select from ZI_TM_ORDEM_CARREG_LISTA_LACRE as lacre
    
    inner join /scmtms/d_torrot as torrot on lacre.root_key = torrot.db_key  
{
    key torrot.tor_id as Orp_Id, 
    lacre.seal_number
}
group by
    torrot.tor_id,
    lacre.seal_number
