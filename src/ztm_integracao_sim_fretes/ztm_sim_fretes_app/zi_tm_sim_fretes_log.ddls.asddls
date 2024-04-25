@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Sim Fretes Log'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SIM_FRETES_LOG as select from zttm_fretes_log
association to parent ZI_TM_SIM_FRETES as _pai on _pai.CtcNumero = $projection.Processo
                                              and _pai.CtcSerie = $projection.Message
{
    key id_processo as IdProcesso,
    key id_msgs as IdMsgs,
    processo as Processo,
    type as Type,
    id as Id,
    numero as Numero,
    message as Message,
    status as Status,
    created_by as CreatedBy,
    created_at as CreatedAt,
    last_changed_by as LastChangedBy,
    last_changed_at as LastChangedAt,
    local_last_changed_at as LocalLastChangedAt,
    _pai
}
