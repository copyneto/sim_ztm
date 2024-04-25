@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Itens J_1BNFLIN'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_TM_ITENS_NOTA as select from j_1bnflin 
    inner join j_1bnfdoc on j_1bnfdoc.docnum = j_1bnflin.docnum 
    
{
    
    key j_1bnflin.docnum,
    
    @Semantics.amount.currencyCode : 'waerk'
    sum(j_1bnflin.netwr) as netwr,
    j_1bnfdoc.waerk
    
}   
 group by j_1bnflin.docnum,
         j_1bnfdoc.waerk
