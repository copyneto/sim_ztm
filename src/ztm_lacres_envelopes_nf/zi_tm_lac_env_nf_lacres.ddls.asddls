@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Lacres'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_LAC_ENV_NF_LACRES
  as select from    /scmtms/d_torite as _torite
    left outer join /scmtms/d_torsl  as _torsl on _torsl.root_key = _torite.parent_key
{
  key _torite.parent_key,
      _torsl.seal_number
}
where
  _torsl.seal_number is not initial
group by
  _torite.parent_key,
  _torsl.seal_number
