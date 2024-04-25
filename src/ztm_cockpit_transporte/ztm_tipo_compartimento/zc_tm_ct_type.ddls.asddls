@EndUserText.label: 'Tipo de Compartimento - Tipos'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZC_TM_CT_TYPE
  provider contract transactional_query
  as projection on ZI_TM_CT_TYPE
{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_TM_VH_SAPAPO_CTYPE', element: 'CompartmentType' } }]
      @EndUserText.label: 'Tipo de compartimento'
  key CompartmentType,
      @EndUserText.label: 'Tipo de compartimento (criticalidade)'
      CompartmentTypeCriticality,
      @EndUserText.label: 'Ordem única'
      UniqueOrder,

      /* Associations */
      _Capacity : redirected to composition child ZC_TM_CT_CAPACITY

}
