@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Motoristas'

define root view entity ZI_TM_MOTORISTAS

  as select from ZI_TM_MOTORISTAS_UNION as _pai

  composition [0..*] of ZI_TM_DOC_ADD as _filho

{
  key _pai.id,
      _pai.BusinessPartnerRedeSim,

      case when _pai.BusinessPartnerRedeSim = 'X'
           then 3
           else 2
           end as BusinessPartnerRedeSimCrit,

      _pai.BusinessPartnerGrouping,
      _pai.BusinessPartnerGroupingText,
      _pai.Cpf,
      _pai.CpfCriticality,
      _pai.Nome,
      _pai.Rg,
      _pai.RgCriticality,
      _pai.Cnh,
      _pai.CnhCriticality,
      _pai.Validadecnh,
      _pai.ValidadecnhCriticality,
      _pai.Categoriacnh,
      _pai.bp,
      _pai.bpCriticality,
      _pai.status,
      @Semantics.user.createdBy: true
      _pai.CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      _pai.CreatedAt,
      @Semantics.user.lastChangedBy: true
      _pai.LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      _pai.LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _pai.LocalLastChangedAt,

      -- Indica se a informação veio do Aplicativo (APP) ou Parceiro (BP)
      StatusReplicacao,

      _filho // Make association public
}
