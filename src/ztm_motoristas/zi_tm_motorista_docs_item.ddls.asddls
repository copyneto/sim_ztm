@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Motorista - Documentos Adicionais'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_MOTORISTA_DOCS_ITEM

  as select from ZI_TM_MOTORISTA_DOCS_ITEM_GRP
{
  key id                       as id,
  key bp                       as bp,
      min( NumeroCriticality ) as NumeroCriticality
}
group by
  id,
  bp
