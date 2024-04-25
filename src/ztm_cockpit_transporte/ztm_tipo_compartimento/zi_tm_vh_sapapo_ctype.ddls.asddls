@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Tipo de Compartimento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_SAPAPO_CTYPE
  as select from /sapapo/ctype
{
      @UI.lineItem       : [{ position: 10 }]
      @UI.identification : [{ position: 10 }]
      @EndUserText.label: 'Tipo de compartimento'
  key ctype                                as CompartmentType,

      @UI.lineItem       : [{ position: 10 }]
      @UI.identification : [{ position: 10 }]
      @EndUserText.label: 'Ordem única'
      cast( unique_order as abap_boolean ) as UniqueOrder
}
