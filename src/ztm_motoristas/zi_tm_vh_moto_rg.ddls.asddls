@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - Rg'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MOTO_RG
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.Rg as Rg
}
where
  _motorista.Rg is not initial
group by
  _motorista.Rg
