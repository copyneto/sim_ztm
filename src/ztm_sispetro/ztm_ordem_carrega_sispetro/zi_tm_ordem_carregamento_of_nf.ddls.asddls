@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Recuperar Ordem de Frete com base na NF'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_OF_NF as 
    select from I_BR_NFDocumentFlow_C as flow
    
    inner join /scmtms/d_torite as torite on flow.PredecessorReferenceDocument = substring(torite.base_btd_id, 26, 35)   
    
    inner join /scmtms/d_torrot as torrot on torite.parent_key = torrot.db_key  
{
    key flow.BR_NotaFiscal as DocNum,
    key torrot.tor_id as OrdemFrete
}
where
    torrot.tor_cat = 'TO' and
    torrot.lifecycle <> '10'
group by
    flow.BR_NotaFiscal,
    torrot.tor_id
    
union

    select  from I_BR_NFItem       
    
    inner join nsdm_e_MKPF as mkpf on mkpf.mblnr = substring( I_BR_NFItem.BR_NFSourceDocumentNumber, 1, 10 )  and
                       mkpf.mjahr = substring( I_BR_NFItem.BR_NFSourceDocumentNumber, 11, 4 )
    
    inner join /scmtms/d_torite as torite on torite.base_btd_id = lpad(mkpf.le_vbeln, 35, '0')
    
    inner join /scmtms/d_torrot as torrot on torite.parent_key = torrot.db_key 
{
    key I_BR_NFItem.BR_NotaFiscal as DocNum,
    key torrot.tor_id as OrdemFrete     
}
where
    torrot.tor_cat = 'TO' and
    torrot.lifecycle <> '10'       
group by
    I_BR_NFItem.BR_NotaFiscal,
    torrot.tor_id       
