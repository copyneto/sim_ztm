@EndUserText.label: 'CDS Proj. - Busca Compartimentos'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_TM_ORDEM_CARREGAMENTO_COMP
  as projection on ZI_TM_ORDEM_CARREGAMENTO_COMP
{
      @EndUserText.label: 'NodeID'
  key db_key,
      @EndUserText.label: 'Parent NodeID'
  key parent_key,
      @EndUserText.label: 'Ordem de Frete'
  key Orp_Id,
      @EndUserText.label: 'NodeID Z'
      zz_fu_db_key,
      @EndUserText.label: 'Compartimento'
      ct_seq,
      @EndUserText.label: 'Utilização'
      zz_fu_max_util,
      @EndUserText.label: 'Produto'
      item_descr,
      @EndUserText.label: 'UN'
      gro_vol_uni,
      @EndUserText.label: 'Volume total'
      gro_vol_val,
      @EndUserText.label: 'Recurso'
      res_id,
      @EndUserText.label: 'Envelope de amostra'
      envelope,
      @EndUserText.label: 'Unidade de Frete'
      unidadeFrete,
      _paiComp : redirected to parent ZC_TM_SISPETRO
}
