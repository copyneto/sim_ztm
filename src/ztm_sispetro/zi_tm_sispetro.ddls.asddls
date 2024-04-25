@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Sispetro'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_TM_SISPETRO
  as select from    ZI_TM_SISPETRO_AUX as _main

    left outer join ZI_TM_SISPETRO_LOG as _log on _log.id_processo = _main.db_key

  association [1..1] to ZI_TM_TORITE                   as _torite    on _torite.parent_key = $projection.db_key

  association [1..1] to zttm_cor_lacres                as _corlacres on _corlacres.db_key = $projection.db_key

  association [1..1] to ZI_TM_ORDEM_CARREG_VOLUME_TOT  as _VolumeTot on _VolumeTot.parent_key = $projection.db_key

  //    left outer join I_BR_NFDocumentFlow_C as _docflow on _docflow.PredecessorReferenceDocument = _main.base_btd_id
  //    left outer join j_1bnfdoc             as _nfdoc   on _nfdoc.docnum = _docflow.BR_NotaFiscal

  composition [0..*] of ZI_TM_SISPETRO_LOG_ALL         as _loga

  composition [0..*] of ZI_TM_SISPETRO_NOTA_FISCAL     as _NotaFiscal

  composition [0..*] of ZI_TM_ORDEM_CARREGAMENTO_COMP  as _Compartimentos

  composition [0..*] of ZI_TM_ORDEM_CARREG_LISTA_LACRE as _Lacres
  //  on _Compartimentos.parent_key = _main.db_key

{
  key _main.db_key,
      _main.OrdemFrete,
      _main.Transportadora,
      _main.taxnumxl,
      //      _main.Centro,
      //      _main.Placavalo,
      //      _main.Placarreta1,
      //      _main.Placarreta2,
      //      _docflow.BR_NotaFiscal,
      //      _nfdoc.docdat,
      _log.id_processo,
      cast( _log.processo as abap.char( 20 ) ) as processo,
      cast( _log.processo as abap.char( 20 ) ) as sispetro,
      _log.last_changed_at,
      _log.message,
      _log.status,
      _torite.Centro,
      _torite.Placavalo,
      _torite.Placarreta1,
      _torite.Placarreta2,
      _torite.SeqFrota4,
      _torite.Motorista,
      _torite.DescMotorista,
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCLTM_VE_CORLACRE'
      cast('' as ze_param_low)                 as CorLacre,
      _VolumeTot.gro_vol_uni,
      @Semantics.quantity.unitOfMeasure: 'gro_vol_uni'
      _VolumeTot.Volumetotal                   as VolumeTotal,
      _main.OrdemFrete as FreightOrder,
      _main.TranspOrdLifeCycleStatus,
      _main.TranspOrdLifeCycleStatusDesc,
      //      _main.Motorista,
      //      _main.DescMotorista,
      _loga,
      _NotaFiscal,
      _Compartimentos,
      _Lacres
}
