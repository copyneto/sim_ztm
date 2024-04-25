@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Sispetro Auxiliar'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SISPETRO_AUX
  as select from /scmtms/d_torrot as _main
  association [0..1] to dfkkbptaxnum                   as _tax    on  _tax.taxtype = 'BR1'
                                                                  and _tax.partner = _main.tspid

  association [0..1] to I_TranspOrdLifeCycleStatusText as _status on  _status.TranspOrdLifeCycleStatus = _main.lifecycle
                                                                  and _status.Language                 = $session.system_language
{
  key _main.db_key,
      _main.tor_id as OrdemFrete,
      _main.tspid  as Transportadora,
      _tax.taxnum  as taxnumxl,

      //      _main._TranspOrdLifeCycleStatus.TranspOrdLifeCycleStatus,
      //      _main._TranspOrdLifeCycleStatus._Text[ Language = $session.system_language ].TranspOrdLifeCycleStatusDesc
      //    _torite.erp_plant_id as Centro,
      //    _torite[ mtr = 'ZMTR-CAV'  and res_seq = 1 ].platenumber as Placavalo,
      //    _torite[ mtr = 'ZMTR-CARR' and res_seq = 2 ].platenumber as Placarreta1,
      //    _torite[ mtr = 'ZMTR-CARR' and res_seq = 3 ].platenumber as Placarreta2,
      //    _torite[ item_cat = 'DRI' ].res_id as Motorista,
      //    _torite[ item_cat = 'DRI' ].item_descr as DescMotorista,
      //    _torite.base_btd_id,
      //    _torite
      _status.TranspOrdLifeCycleStatus,
      _status.TranspOrdLifeCycleStatusDesc
}
where
  _main.tor_cat = 'TO'
//  and _main.lifecycle <> '10'
