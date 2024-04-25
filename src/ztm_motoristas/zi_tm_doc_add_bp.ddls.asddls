@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Doc Adicionais (BP)'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_DOC_ADD_BP

  as select from    ZI_TM_DOC_ADD_BP_TM as _partner

    left outer join zttm_doc_add        as _app on  _app.id   = _partner.id
                                                and _app.tipo = _partner.Tipo
{
  key _partner.id,
  key _partner.paiid,
      _partner.bp,
      _partner.Tipo,
      _partner.Numero,
      _partner.NumeroCriticality,
      _partner.ValidadeDesde,
      _partner.Validade,
      _partner.Uf,
      _partner.Descricao,
      @Semantics.user.createdBy: true
      _partner.CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      _partner.CreatedAt,
      @Semantics.user.lastChangedBy: true
      _partner.LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      _partner.LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _partner.LocalLastChangedAt

}
where
  _app.id is null -- Somente os parceiros que não foram modificados pelo app
