@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Impressão de Romaneio'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZI_TM_IMPRESSAO_DE_ROMANEIO as select from j_1bnfdoc 
    association[1..*] to ZI_TM_ITENS_NOTA as _1BNFLIN on _1BNFLIN.docnum = $projection.docnum
    association[1..1] to ZI_TM_PEDIDO_NOTA as _Pedido on _Pedido.docnum = $projection.docnum
                                    
{
   // as VOL, as Cliente,
   key docnum,
   //  Transportadora, // Nome da transportadora, buscar do agente de frete da remessa.
   nfenum as Nota,     // número da nota fiscal
   docdat as Emissao, // data da emissão
   anzpk as VOL,         // quantidade, buscar da tabela J_1BNFDOC 
   _Pedido.Pedido,      // número da ordem de venda, buscar da tabela VBAK docref
   @Semantics.quantity.unitOfMeasure: 'UnidadePeso'
   ntgew as Peso,     // peso liquido,
   @Semantics.amount.currencyCode: 'Moeda'
   _1BNFLIN.netwr as Valor,    // valor da nota fiscal, buscar o campo NTOT da tabela J_1BNFDOC
   name1 as Cliente, // nome do cliente, buscar o campo NAME1 da tabela J_1BNFDOC
   ort01 as Cidade, // cidade, buscar o campo ORT01 da tabela J_1BNFDOC
   regio as UF,              // estado, buscar o campo REGIO da tabela J_1BNFDOC    
   shpmrk,
   shpnum,
   gewei as  UnidadePeso, 
   vol_unit,
   waerk as Moeda,
   _Pedido.Transportadora,
   _Pedido.TransportadoraDesc
 
}
where cancel = ''


 
