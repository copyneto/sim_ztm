@EndUserText.label: 'CDS de Projeção - Motoristas'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_TM_MOTORISTAS
  provider contract transactional_query
  as projection on ZI_TM_MOTORISTAS
{
  key id,
      @EndUserText.label: 'Funcionário SIM REDE'
      BusinessPartnerRedeSim,
      BusinessPartnerRedeSimCrit,
      @EndUserText.label: 'Parceiro de negócios'
      @ObjectModel.text.element: [ 'BusinessPartnerGroupingText' ]
      BusinessPartnerGrouping,
      BusinessPartnerGroupingText,
      @EndUserText.label: 'CPF'
      Cpf,
      @EndUserText.label: 'CPF (cor)' 
      CpfCriticality,
      @EndUserText.label: 'Nome'
      Nome,
      @EndUserText.label: 'RG'
      Rg,
      @EndUserText.label: 'RG (cor)'
      RgCriticality,
      @EndUserText.label: 'CNH'
      Cnh,
      @EndUserText.label: 'CNH (cor)'
      CnhCriticality,
      @EndUserText.label: 'Validade CNH'
      Validadecnh,
      @EndUserText.label: 'Validade CNH (cor)'
      ValidadecnhCriticality,
      @EndUserText.label: 'Categoria CNH'
      Categoriacnh,
      @EndUserText.label: 'BP'
      bp,
      @EndUserText.label: 'BP (cor)'
      bpCriticality,
      @EndUserText.label: 'Status'
      status,

      @EndUserText.label: 'Criado por'
      CreatedBy,
      @EndUserText.label: 'Criado em'
      CreatedAt,
      @EndUserText.label: 'Atualizado por'
      LastChangedBy,
      @EndUserText.label: 'Atualizado em'
      LastChangedAt,
      @EndUserText.label: 'Registrado em'
      LocalLastChangedAt,

      @EndUserText.label: 'Status da edição'
      StatusReplicacao,

      //      btnCad,

      /* Associations */
      _filho : redirected to composition child ZC_TM_DOC_ADD
}
