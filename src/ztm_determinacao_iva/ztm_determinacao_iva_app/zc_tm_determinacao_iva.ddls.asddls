@EndUserText.label: 'Determinação de IVA'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_TM_DETERMINACAO_IVA
  provider contract transactional_query
  as projection on ZI_TM_DETERMINACAO_IVA as _Determinacao
{
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZI_TM_CENARIO_VH', element: 'Domname' }}]
      @EndUserText.label: 'Cenário'
  key Cenario,
      @EndUserText.label: 'UF Origem'
  key Uforigem,
      @EndUserText.label: 'UF Destino'
  key Ufdestino,
      @EndUserText.label: 'UF Transportadora'
  key Uftransp,
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_BusinessPartnerVH', element: 'BusinessPartner' }}]
      @EndUserText.label: 'Transportadora (BP)'
      Transportadora,
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_BusinessPartnerVH', element: 'BusinessPartner' }}]
      @EndUserText.label: 'Cliente/Fornecedor (BP)'
      Clientefornec,
      @EndUserText.label: 'IVA'
      Iva
      
}
