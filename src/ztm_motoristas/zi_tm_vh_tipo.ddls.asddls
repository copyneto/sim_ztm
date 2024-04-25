@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Value Help - Tipo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZI_TM_VH_TIPO
  as select from ZI_CA_GET_PARAMETER
{
  key cast( Low as bu_id_type ) as type
}
where
      Chave1 = 'MOTORISTAS'
  and Chave2 = 'DOCUMENTOS'
  and Modulo = 'TM'
group by
  Low
