@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Número do Lacre'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_TOR_SEAL_NUMBER
  as select from /scmtms/d_torsl as _seal

  association [0..1] to /scmtms/d_torrot as _root on _root.db_key = _seal.root_key
{
  key _seal.seal_number        as seal_number,
  key _seal.zz_fu_erp_plant_id as erp_plant_id,
  key _seal.db_key             as db_key,
      _seal.root_key           as root_key,
      _root.tor_id             as tor_id,
      _seal.parent_key         as parent_key
}

where
  _root.lifecycle <> '10' -- Cancelado
