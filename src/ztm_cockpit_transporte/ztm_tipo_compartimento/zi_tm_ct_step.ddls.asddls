@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipo de Compartimento - Níveis'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_CT_STEP
  as select from /sapapo/ctstep

  association [0..1] to ZI_TM_CT_TYPE            as _Type     on  _Type.CompartmentType = $projection.CompartmentType

  association        to parent ZI_TM_CT_CAPACITY as _Capacity on  _Capacity.CompartmentType = $projection.CompartmentType
                                                              and _Capacity.CompartmentUnit = $projection.CompartmentUnit
{
      @EndUserText.label: 'Tipo de compartimento'
  key ctype                            as CompartmentType,
      @EndUserText.label: 'Unidade de medida'
  key ctunit                           as CompartmentUnit,
      @EndUserText.label: 'Nível'
  key step_id                          as StepId,
      @EndUserText.label: 'Capacidade'
      @Semantics.quantity.unitOfMeasure : 'CompartmentUnit'
      ctcapa                           as CompartmentCapacity,
      @EndUserText.label: 'Linear'
      cast( flexible as abap_boolean ) as Flexible,

      /* Associations */
      _Type,
      _Capacity
}
