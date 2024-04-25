@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status ordem de frete'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_STATUS_OF
  as select from I_TranspOrdLifeCycleStatusText
{
  key TranspOrdLifeCycleStatus,
      TranspOrdLifeCycleStatusDesc
}
where
  Language = $session.system_language
