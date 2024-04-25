@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca Notas Fiscais'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SISPETRO_NOTA_FISCAL
  as select from    /scmtms/d_torite      as _torite
    left outer join I_BR_NFDocumentFlow_C as _nf     on _nf.PredecessorReferenceDocument = substring(
      _torite.base_btd_id, 26, 35
    )
    left outer join j_1bnfdoc             as _dat    on _dat.docnum = _nf.BR_NotaFiscal
    inner join      j_1bnfe_active        as _Active on  _Active.docnum =  _dat.docnum
                                                     and _Active.cancel <> 'X'

  association to parent ZI_TM_SISPETRO as _paiNotaFiscal on $projection.parent_key = _paiNotaFiscal.db_key
{
  key _torite.parent_key,
  key _nf.BR_NotaFiscal,
  key _nf.BR_NotaFiscalItem,
      _dat.docdat as docdat,
      _torite.base_btd_id,
      _dat.authdate,


      _paiNotaFiscal
}
where
      _nf.BR_NotaFiscal is not initial
  and _torite.item_type = 'PRD'
group by
  _torite.parent_key,
  _nf.BR_NotaFiscal,
  _nf.BR_NotaFiscalItem,
  _dat.docdat,
  _dat.authdate,
  _torite.base_btd_id
