@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Coleta de Amostra Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_COL as 
    select from /scmtms/d_torrot as torrot 
    
    inner join  /scmtms/d_torite as torite on torrot.db_key = torite.parent_key
                                          and torite.item_cat = 'CT'
{
    key torrot.tor_id as Orp_Id,
    torite.ct_seq,
    cast(concat(concat('CT-', torite.ct_seq), concat(' Amostra-', torite.zz_sample_envelope)) as abap.char( 400 )) as ColetaAmostra            
}
where 
    torite.ct_seq is not initial and
    torite.zz_sample_envelope is not initial
group by
    torrot.tor_id,
    torite.ct_seq,
     torite.zz_sample_envelope
