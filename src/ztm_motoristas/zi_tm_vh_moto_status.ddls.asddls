@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - Status'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MOTO_STATUS
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.status as status
}
where
  _motorista.status is not initial
group by
  _motorista.status
