@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Placa Cavalo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_PLACA_CAVALO as select from /scmtms/d_torite
{   
    key parent_key,
    platenumber
}where mtr = 'ZMTR-TRK'
and item_type= 'TRUC'
and item_cat= 'AVR'
group by parent_key,
         platenumber
union all select from /scmtms/d_torite
{
    key parent_key,
    platenumber
}where mtr = 'ZMTR-CAV' and res_seq = 1
group by parent_key,
         platenumber
