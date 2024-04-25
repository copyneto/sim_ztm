@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Compartimentos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_COMP
  as select from    /scmtms/d_torrot             as _Torrot

    inner join      /scmtms/d_torite             as _CompCT  on _Torrot.db_key = _CompCT.parent_key
    left outer join ZI_TM_TORITE                 as _TorIte  on _TorIte.parent_key = _Torrot.db_key

    left outer join ZI_TM_ORDEM_CARREGA_COMP_PRD as _CompPRD on _CompCT.zz_fu_db_key = _CompPRD.parent_key
  association to parent ZI_TM_SISPETRO as _paiComp on $projection.parent_key = _paiComp.db_key
{
  key _CompCT.db_key                                    as db_key,
  key _CompCT.parent_key                                as parent_key,
  key _Torrot.tor_id                                    as Orp_Id,
      _CompCT.zz_fu_db_key                              as zz_fu_db_key,
      _CompCT.ct_seq                                    as ct_seq,
      _CompCT.zz_fu_max_util                            as zz_fu_max_util,
      _CompPRD.item_descr                               as item_descr,
      _CompPRD.gro_vol_uni                              as gro_vol_uni,
      @Semantics.quantity.unitOfMeasure: 'gro_vol_uni'
      cast(_CompPRD.gro_vol_val as abap.quan( 31, 14 )) as gro_vol_val,
      _CompCT.res_id,
      max(_CompCT.zz_sample_envelope)                   as envelope,
      _CompCT.zz_fu_tor_id as unidadeFrete,
      _paiComp
}
where
  _CompCT.item_cat = 'CT'
group by
  _CompCT.db_key,
  _CompCT.parent_key,
  _Torrot.tor_id,
  _CompCT.zz_fu_db_key,
  _CompCT.ct_seq,
  _CompCT.zz_fu_max_util,
  _CompPRD.item_descr,
  _CompPRD.gro_vol_uni,
  _CompPRD.gro_vol_val,
  _CompCT.res_id,
  _CompCT.zz_fu_tor_id
//  _CompCT.zz_sample_envelope
