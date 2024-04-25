@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Fluxo caixa (BP)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_MOTORISTAS_BP

  as select from ZI_TM_MOTORISTA_DOCS as _docs

  association [0..1] to zttm_motoristas as _app on _app.id = $projection.id

{
  key _docs.partner_guid                as id,

      _docs.BusinessPartnerRedeSim      as BusinessPartnerRedeSim,
      _docs.BusinessPartnerGrouping     as BusinessPartnerGrouping,
      _docs.BusinessPartnerGroupingText as BusinessPartnerGroupingText,

      _docs.cpf                         as Cpf,
      cast( 0 as abap.int1 )            as CpfCriticality,
      _docs.partnername                 as Nome,
      _docs.rg                          as Rg,
      cast( 0 as abap.int1 )            as RgCriticality,
      _docs.cnh                         as Cnh,
      cast( 0 as abap.int1 )            as CnhCriticality,
      _docs.Validadecnh                 as Validadecnh,

      case when dats_days_between( $session.system_date, _docs.Validadecnh ) > 0
            and dats_days_between( $session.system_date, _docs.Validadecnh ) <= 30
           then 2
           when dats_days_between( $session.system_date, _docs.Validadecnh ) < 0
           then 1
           else 0
           end                          as ValidadecnhCriticality,

      _docs.Categoriacnh                as Categoriacnh,
      _docs.partner                     as bp,
      cast( 3 as abap.int1 )            as bpCriticality,
      cast( '' as bapi_msg )            as status,

      @Semantics.user.createdBy: true
      _docs.CreatedBy                   as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      _docs.CreatedAt                   as CreatedAt,
      @Semantics.user.lastChangedBy: true
      _docs.LastChangedBy               as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      _docs.LastChangedAt               as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _docs.LocalLastChangedAt          as LocalLastChangedAt

}
where
      _docs.TM = 'X' -- Somente os parceiros com função TM
  and _app.id  is null -- Somente os parceiros que não foram modificados pelo app
