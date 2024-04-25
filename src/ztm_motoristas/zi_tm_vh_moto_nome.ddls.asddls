@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - Nome'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MOTO_NOME
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.Nome as Nome
}
where
  _motorista.Nome is not initial
group by
  _motorista.Nome
