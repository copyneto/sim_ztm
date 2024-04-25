@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - CPF'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MOTO_CPF
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.Cpf as Cpf
}
where
  _motorista.Cpf is not initial
group by
  _motorista.Cpf
