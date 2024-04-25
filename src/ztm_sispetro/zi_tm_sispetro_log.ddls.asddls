@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Sispetro Log'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SISPETRO_LOG as select from zttm_logsispetr0 as _log
inner join ZI_TM_SISPETRO_LAST_LOG as _ll on _ll.id_processo = _log.id_processo
                                         and _ll.last_changed_at = _log.last_changed_at
{
    key _log.id_processo,
    key _log.id_msgs,
    _log.processo,
    _log.last_changed_at,
    _log.message,
    _log.status
}
