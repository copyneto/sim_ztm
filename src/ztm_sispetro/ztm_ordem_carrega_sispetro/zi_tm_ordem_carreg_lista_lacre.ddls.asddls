@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Lista de Lacres'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREG_LISTA_LACRE
  as select from /scmtms/d_torsl
  association to parent ZI_TM_SISPETRO as _paiLacre on $projection.root_key = _paiLacre.db_key
{
    key db_key,
    key root_key,
        seal_number,
        _paiLacre

  }
