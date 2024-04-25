@EndUserText.label: 'Tipo de Compartimento - Capacidades'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_TM_CT_CAPACITY
  as projection on ZI_TM_CT_CAPACITY
{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_TM_VH_SAPAPO_CTYPE', element: 'CompartmentType' } }]
      @EndUserText.label: 'Tipo de compartimento'
  key CompartmentType,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_TM_VH_MEINS', element: 'UnitOfMeasure' } }]
      @EndUserText.label: 'Unidade de medida'
  key CompartmentUnit,
      @EndUserText.label: 'Capacidade'
      CompartmentCapacity,

      /* Associations */
      _Type : redirected to parent ZC_TM_CT_TYPE,
      _Step : redirected to composition child ZC_TM_CT_STEP
}
