@EndUserText.label: 'Tipo de Compartimento - Níveis'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_TM_CT_STEP
  as projection on ZI_TM_CT_STEP
{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_TM_VH_SAPAPO_CTYPE', element: 'CompartmentType' } }]
      @EndUserText.label: 'Tipo de compartimento'
  key CompartmentType,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_TM_VH_MEINS', element: 'UnitOfMeasure' } }]
      @EndUserText.label: 'Unidade de medida'
  key CompartmentUnit,
      @EndUserText.label: 'Nível'
  key StepId,
      @EndUserText.label: 'Capacidade'
      CompartmentCapacity,
      @EndUserText.label: 'Linear'
      Flexible,

      /* Associations */
      _Type     : redirected to ZC_TM_CT_TYPE,
      _Capacity : redirected to parent ZC_TM_CT_CAPACITY
}
