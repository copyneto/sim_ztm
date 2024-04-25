@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Motorista - Categoria CNH'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZI_TM_VH_MOTO_CATEGORIA_CNH
  as select from ZI_TM_MOTORISTAS_UNION as _motorista
{
  key _motorista.Categoriacnh as Categoriacnh
}
where
  _motorista.Categoriacnh is not initial
group by
  _motorista.Categoriacnh
