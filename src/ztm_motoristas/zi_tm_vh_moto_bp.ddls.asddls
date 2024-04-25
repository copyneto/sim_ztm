@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - Parceiro neg.'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MOTO_BP
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.bp as bp
}
where
  _motorista.bp is not initial
group by
  _motorista.bp
