@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agente de Frete pela Remessa'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_AGENTE_FRETE_NF
  as select from /scmtms/d_torite as Torite
    inner join   /scmtms/d_torrot as Torrot on  Torrot.db_key  = Torite.parent_key
                                            and Torrot.tor_cat = 'TO'
{
  key cast( LPAD(ltrim(Torite.base_btd_id, '0'), 10, '0') as vbeln_vl ) as Vbeln,
  key Torrot.tor_id                                                     as OrdFrete,
      Torrot.tspid                                                      as AgtFrete

}
group by
  Torite.base_btd_id,
  Torrot.tor_id,
  Torrot.tspid
