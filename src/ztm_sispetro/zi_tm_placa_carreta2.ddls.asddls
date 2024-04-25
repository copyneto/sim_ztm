@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Placa Carreta 2'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_PLACA_CARRETA2 as select from /scmtms/d_torite
{
    key parent_key,
    platenumber
}where mtr = 'ZMTR-CARR' and res_seq = 3
group by parent_key,
         platenumber
