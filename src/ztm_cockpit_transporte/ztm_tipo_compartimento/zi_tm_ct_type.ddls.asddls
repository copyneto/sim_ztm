@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipo de Compartimento - Tipos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_TM_CT_TYPE
  as select from /sapapo/ctype

  composition [0..*] of ZI_TM_CT_CAPACITY  as _Capacity

  association [0..1] to ZI_TM_CT_TYPE_USED as _UsedType on _UsedType.CompartmentType = $projection.CompartmentType
{
      @EndUserText.label: 'Tipo de compartimento'
  key ctype                                as CompartmentType,

      case when _UsedType.CompartmentType is not null
            and _UsedType.CompartmentType is not initial
           then 3
           else 0
           end                             as CompartmentTypeCriticality,

      @EndUserText.label: 'Ordem única'
      cast( unique_order as abap_boolean ) as UniqueOrder,

      /* Associations */
      _Capacity
}
