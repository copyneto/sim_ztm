@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Unidade de Medida'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_VH_MEINS
  as select from I_UnitOfMeasure
{
  key UnitOfMeasure                                                            as UnitOfMeasure,
      _Text[ 1:Language = $session.system_language].UnitOfMeasureName          as UnitOfMeasureName,
      _Text[ 1:Language = $session.system_language].UnitOfMeasureTechnicalName as UnitOfMeasureTechnicalName
}
