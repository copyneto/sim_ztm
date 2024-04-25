@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Doc Adicionais (App e BP)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_DOC_ADD_UNION
  as select from ZI_TM_DOC_ADD_APP
{
  key id,
  key paiid,
      bp,
      Tipo,
      Numero,
      NumeroCriticality,
      ValidadeDesde,
      Validade,
      Uf,
      Descricao,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      cast( 'APP' as ze_tm_status_replicacao ) as StatusReplicacao
}
union select from ZI_TM_DOC_ADD_BP
{
  key id,
  key paiid,
      bp,
      Tipo,
      Numero,
      NumeroCriticality,
      ValidadeDesde,
      Validade,
      Uf,
      Descricao,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      cast( 'BP' as ze_tm_status_replicacao )  as StatusReplicacao
}
