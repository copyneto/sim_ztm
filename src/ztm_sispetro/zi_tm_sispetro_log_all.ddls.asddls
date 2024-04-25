@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Sispetro Log'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SISPETRO_LOG_ALL as select from ZTTM_LOGSISPETR0 as _log
association to parent ZI_TM_SISPETRO as _pai on $projection.id_processo = _pai.db_key
{
    key _log.id_processo,
    key _log.id_msgs,
    _log.processo,
    _log.last_changed_at,
    _log.message,
    _log.status,
    _pai
}
