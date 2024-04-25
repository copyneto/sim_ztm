@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Sispetro Último Log'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SISPETRO_LAST_LOG as select from zttm_logsispetr0
{
    key id_processo,
    max(last_changed_at) as last_changed_at
}group by id_processo
