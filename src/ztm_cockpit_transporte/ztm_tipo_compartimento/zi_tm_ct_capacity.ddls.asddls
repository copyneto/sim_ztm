@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipo de Compartimento - Capacidades'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_CT_CAPACITY
  as select from /sapapo/ctcap

  association to parent ZI_TM_CT_TYPE as _Type on _Type.CompartmentType = $projection.CompartmentType

  composition [0..*] of ZI_TM_CT_STEP as _Step

{
      @EndUserText.label: 'Tipo de compartimento'
  key ctype  as CompartmentType,
      @EndUserText.label: 'Unidade de medida'
  key ctunit as CompartmentUnit,
      @EndUserText.label: 'Capacidade'
      @Semantics.quantity.unitOfMeasure : 'CompartmentUnit'
      ctcapa as CompartmentCapacity,

      /* Associations */
      _Type,
      _Step
}
