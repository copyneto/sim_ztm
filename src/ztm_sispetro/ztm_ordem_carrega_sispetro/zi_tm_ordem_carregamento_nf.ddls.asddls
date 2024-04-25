@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Nota Fiscal Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_NF
  as

  select from  /scmtms/d_torrot      as torrot

    inner join /scmtms/d_torite      as torite on torrot.db_key = torite.parent_key

  //inner join I_BR_NFDocumentFlow_C as nf on torite.base_btd_id = nf.PredecessorReferenceDocument

    inner join I_BR_NFDocumentFlow_C as nf     on nf.PredecessorReferenceDocument = substring(
      torite.base_btd_id, 26, 35
    )
    inner join I_BR_NFeActive        as active on  active.BR_NotaFiscal   = nf.BR_NotaFiscal
                                               and active.BR_NFIsCanceled is initial
{
  key torrot.tor_id                                                        as Orp_Id,
  key nf.BR_NotaFiscal,
      max( concat(concat(active.BR_NFeNumber,'/'), active.BR_NFeSeries ) ) as Nfe
}
group by
  torrot.tor_id,
  nf.BR_NotaFiscal
