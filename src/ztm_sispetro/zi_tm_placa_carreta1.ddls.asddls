@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Placa Carreta 1'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_PLACA_CARRETA1 as select from /scmtms/d_torite
{
    key parent_key,
    platenumber
}where mtr = 'ZMTR-CARR' and res_seq = 2
group by parent_key,
         platenumber
