@EndUserText.label: 'CDS Proj. - Busca Lista de Lacres'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_TM_ORDEM_CARREG_LISTA_LACRE
  as projection on ZI_TM_ORDEM_CARREG_LISTA_LACRE
{
  key db_key,
      @EndUserText.label: 'NodeID'
  key root_key,
      @EndUserText.label: 'Nº do Lacre'
      seal_number,
      
      _paiLacre :  redirected to parent ZC_TM_SISPETRO
}
