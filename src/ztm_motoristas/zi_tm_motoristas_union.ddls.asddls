@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fluxo caixa (App e BP)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_MOTORISTAS_UNION
  as select from ZI_TM_MOTORISTAS_APP
{
  key id,
      BusinessPartnerRedeSim,
      BusinessPartnerGrouping,
      BusinessPartnerGroupingText,
      cast( Cpf as abap.char(60) )             as Cpf,
      CpfCriticality,
      cast( Nome as abap.char(81) )            as Nome,
      cast( Rg as abap.char(60) )              as Rg,
      RgCriticality,
      cast( Cnh as abap.char(60) )             as Cnh,
      CnhCriticality,
      Validadecnh,
      ValidadecnhCriticality,
      cast( Categoriacnh as abap.char(40) )    as Categoriacnh,
      bp,
      bpCriticality,
      status,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      cast( 'APP' as ze_tm_status_replicacao ) as StatusReplicacao
}
union select from ZI_TM_MOTORISTAS_BP
{
  key id,
      BusinessPartnerRedeSim,
      BusinessPartnerGrouping,
      BusinessPartnerGroupingText,
      Cpf,
      CpfCriticality,
      Nome,
      Rg,
      RgCriticality,
      Cnh,
      CnhCriticality,
      Validadecnh,
      ValidadecnhCriticality,
      Categoriacnh,
      bp,
      bpCriticality,
      status,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      cast( 'BP' as ze_tm_status_replicacao ) as StatusReplicacao
}
