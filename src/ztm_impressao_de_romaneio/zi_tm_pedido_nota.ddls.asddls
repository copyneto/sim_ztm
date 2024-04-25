@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Ordem de Venda a partir da  Nota Fiscal'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_PEDIDO_NOTA as select from j_1bnflin 
    inner join j_1bnfdoc on j_1bnfdoc.docnum = j_1bnflin.docnum 
    
    inner join vbfa as _vbfa on _vbfa.vbeln = j_1bnflin.refkey
                            and _vbfa.posnn = j_1bnflin.refitm
    
    inner join vbak as _vbak on _vbak.vbeln = _vbfa.vbelv
    
    left outer join vbpa as _vbpa on _vbpa.vbeln = _vbak.vbeln
                                  and _vbpa.parvw = 'SP'
    left outer join lfa1 as _Lfa1 on _Lfa1.lifnr = _vbpa.lifnr
{
    
    key j_1bnflin.docnum,
    
    _vbak.vbeln as Pedido, 
    _vbpa.parvw as Funcao,
    _vbpa.lifnr as Transportadora,
    _Lfa1.name1 as TransportadoraDesc
    
}
    group by j_1bnflin.docnum, _vbak.vbeln, _vbpa.parvw, _vbpa.lifnr, _Lfa1.name1
