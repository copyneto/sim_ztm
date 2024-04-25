@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Fluxo caixa (App)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_MOTORISTAS_APP

  as select from zttm_motoristas as _motorista

  association [0..1] to ZI_TM_MOTORISTA_DOCS as _docs on _docs.partner = $projection.bp
{

  key id                                as id,

      case when _docs.BusinessPartnerRedeSim is not null
           then _docs.BusinessPartnerRedeSim
           else cast( _motorista.businesspartnerredesim as abap_boolean )
           end                          as BusinessPartnerRedeSim,


      _docs.BusinessPartnerGrouping     as BusinessPartnerGrouping,
      _docs.BusinessPartnerGroupingText as BusinessPartnerGroupingText,

      _motorista.cpf                    as Cpf,

      case when _docs.cpf is null               -- Se Parceiro não existir
             or _docs.cpf is initial                -- Se CPF estiver vazio
           then 1                                   -- Cor: Vermelho
           when _docs.cpf <> _motorista.cpf     -- Se CPF estiver divergente
           then 2                                   -- Cor: Amarelo
           else 3                                   -- Cor: Verde
           end                          as CpfCriticality,

      _motorista.nome                   as Nome,
      _motorista.rg                     as Rg,

      case when _docs.rg is null                -- Se Parceiro não existir
             or _docs.rg is initial             -- Se RG estiver vazio
           then 1                                   -- Cor: Vermelho
           when _docs.rg <> _motorista.rg       -- Se RG estiver divergente
           then 2                                   -- Cor: Amarelo
           else 3                                   -- Cor: Verde
           end                          as RgCriticality,

      _motorista.cnh                    as Cnh,

      case when _docs.cnh is null                           -- Se Parceiro não existir
             or _docs.cnh is initial                        -- Se CNH estiver vazio
           then 1                                               -- Cor: Vermelho
           when _docs.cnh         <> _motorista.cnh         -- Se CNH estiver divergente
             or _docs.Validadecnh <> _motorista.validadecnh -- Se validade CNH estiver divergente
           then 2                                               -- Cor: Amarelo
           else 3                                               -- Cor: Verde
           end                          as CnhCriticality,

      _motorista.validadecnh            as Validadecnh,

      case when dats_days_between( $session.system_date, _motorista.validadecnh ) > 0
            and dats_days_between( $session.system_date, _motorista.validadecnh ) <= 30
           then 2
           when dats_days_between( $session.system_date, _motorista.validadecnh ) < 0
           then 1
           else 0
           end                          as ValidadecnhCriticality,

      _motorista.categoriacnh           as Categoriacnh,
      _motorista.bp                     as bp,
      cast( 2 as abap.int1 )            as bpCriticality,

      _motorista.status                 as status,

      @Semantics.user.createdBy: true
      created_by                        as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                        as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by                   as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at                   as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at             as LocalLastChangedAt

}
