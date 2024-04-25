@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Carga de combinações de veículos em massa'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_TM_COMBINACAO_RECURSOS
  as select from zttm_comb_recur as _Rec

  association [1..1] to zttm_log_res_com as _Log on  _Log.combination_resource_id = $projection.combination_resource_id
                                                 and _Log.resource_id             = $projection.resource_id
{

  key _Rec.combination_resource_id as combination_resource_id,
  key _Rec.seq_num                 as SeqNum,
      _Rec.equi_type               as EquiType,
      _Rec.equi_code               as EquiCode,
      _Rec.resource_id             as resource_id,
      _Log.message                 as LogMessage,
      _Log.msgty                   as LogType,
      '' as Botao
}
