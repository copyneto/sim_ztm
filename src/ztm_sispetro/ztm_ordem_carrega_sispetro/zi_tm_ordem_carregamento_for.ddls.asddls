@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fornecedor Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO_FOR 
  with parameters
    p_taxtype : bptaxtype 

as 
    select from /scmtms/d_torrot as torrot
    
    inner join  /scmtms/d_torite    as torite on torrot.db_key = torite.parent_key 
    
    inner join  t001w as centro on torite.erp_plant_id = centro.werks 
    
    inner join  dfkkbptaxnum  as fornecedor on centro.lifnr = fornecedor.partner
    
{
    key torrot.tor_id as Orp_Id,
    key torite.erp_plant_id as Fornecedor,
    fornecedor.taxnum as CpfCnpjFornecedor,
    centro.name1 as NomeFornecedor    
}
where 
    torite.erp_plant_id is not initial and
    fornecedor.taxtype = $parameters.p_taxtype
group by
    torrot.tor_id,
    torite.erp_plant_id,
    fornecedor.taxnum,
    centro.name1    
