@EndUserText.label: 'CDS Proj. - Busca Notas Fiscais'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_TM_SISPETRO_NOTA_FISCAL
  as projection on ZI_TM_SISPETRO_NOTA_FISCAL
{
  key parent_key,
      @EndUserText.label: 'Nota Fiscal'
  key BR_NotaFiscal,
      @EndUserText.label: 'Item'
  key BR_NotaFiscalItem,
      @EndUserText.label: 'Data NF'
      docdat,
      @EndUserText.label: 'Remessa'
      base_btd_id,
      

      /* Associations */
      _paiNotaFiscal : redirected to parent ZC_TM_SISPETRO
}
