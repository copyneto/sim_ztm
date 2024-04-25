@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Consumo - Torite'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_TORITE_AUX
  as select from    /scmtms/d_torite     as _torite
    left outer join /scmtms/d_torite     as _motorista   on  _motorista.parent_key = _torite.parent_key
                                                         and _motorista.item_cat   = 'DRI'
    left outer join ZI_TM_PLACA_CAVALO   as _placavalo   on _placavalo.parent_key = _torite.parent_key
    left outer join ZI_TM_PLACA_CARRETA1 as _placarreta1 on _placarreta1.parent_key = _torite.parent_key
    left outer join ZI_TM_PLACA_CARRETA2 as _placarreta2 on _placarreta2.parent_key = _torite.parent_key
    left outer join ZI_TM_PLACA_SEQFROTA4 as _seqFrota4  on _seqFrota4.parent_key = _torite.parent_key


{
      //  key _torite.db_key as torite_key,
  key _torite.parent_key,
      _torite.erp_plant_id     as Centro,
      _placavalo.platenumber   as Placavalo,
      _placarreta1.platenumber as Placarreta1,
      _placarreta2.platenumber as Placarreta2,
      _seqFrota4.platenumber   as SeqFrota4,
      _motorista.res_id        as Motorista,
      _motorista.item_descr    as DescMotorista


      //    _torite.base_btd_id
}
where
  _torite.item_cat = 'PRD'
group by
  _torite.parent_key,
  _torite.erp_plant_id,
  _placavalo.platenumber,
  _placarreta1.platenumber,
  _placarreta2.platenumber,
  _seqFrota4.platenumber,
  _motorista.res_id,
  _motorista.item_descr
