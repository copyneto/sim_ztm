@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - CNH'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MOTO_CNH
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.Cnh as Cnh
}
where
  _motorista.Cnh is not initial
group by
  _motorista.Cnh
