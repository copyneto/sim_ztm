@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_TM_VH_Tipo_Compartimento as select from /sapapo/ctcap {
    key ctype as CType,
    @Semantics.quantity.unitOfMeasure : 'CTUnit'
    ctcapa as CTCapa,
    ctunit as CTUnit
}
