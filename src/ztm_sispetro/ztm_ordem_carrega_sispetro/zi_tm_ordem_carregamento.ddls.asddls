@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ordem de Carregamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_ORDEM_CARREGAMENTO as 
    
    select from         /scmtms/d_torrot    as torrot       
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_COR as cor on torrot.db_key = cor.db_key
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_BASE as base on torrot.tor_id = base.Orp_Id 
                                                                                                                                                                                                                          
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_INC as inco on torrot.tor_id = inco.Orp_Id  
                                                                                               
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_FOR( p_taxtype: 'BR1' ) as fornecedorCnpj on torrot.tor_id = fornecedorCnpj.Orp_Id
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_FOR( p_taxtype: 'BR2' ) as fornecedorCpf on torrot.tor_id = fornecedorCpf.Orp_Id                                                                                                                           

    left outer join     dfkkbptaxnum        as transportador on torrot.tspid = transportador.partner
                                                            and transportador.taxtype = 'BR1'
                                                            
    left outer join     but000 as transp on transportador.partner = transp.partner                                                                                                                             
    
    left outer join     ZI_TM_ORDEM_CARREGAMEN_PLACA_1 as placa1 on torrot.tor_id = placa1.Orp_Id
                                                                 
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_PLACA( p_mtr: 'ZMTR-CARR', p_res_seq: 2 ) as placa2 on torrot.tor_id = placa2.Orp_Id 
                                                                                                    
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_PLACA( p_mtr: 'ZMTR-CARR', p_res_seq: 3 ) as placa3 on torrot.tor_id = placa3.Orp_Id
                                                                                                     
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_PLACA( p_mtr: 'ZMTR-CARR', p_res_seq: 4 ) as placa4 on torrot.tor_id = placa4.Orp_Id 
                                                                                                                                                                                                        
                                                                                                                                                                
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_LACRE as lacre on torrot.tor_id = lacre.Orp_Id  
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_MOT as motorista on torrot.tor_id = motorista.Orp_Id  
    
    left outer join     ZI_TM_ORDEM_CARREGAMENTO_VOL as volume on torrot.tor_id = volume.Orp_Id                                                                                                                         
                                                                                                                                                                                                                                       
{
    
    key torrot.tor_id as Orp_Id,    
    torrot.created_on as DtHr_Emissao,
    inco.Incoterms,
    base.BaseText as BaseText,
    base.BaseText as LocalText,
    case when
        fornecedorCnpj.Fornecedor is not initial
        then fornecedorCnpj.Fornecedor
        else fornecedorCpf.Fornecedor end as Fornecedor,  

    case when
        fornecedorCnpj.CpfCnpjFornecedor is not initial
        then fornecedorCnpj.CpfCnpjFornecedor
        else fornecedorCpf.CpfCnpjFornecedor end as CpfCnpjFornecedor,        
    
    case when
        fornecedorCnpj.NomeFornecedor is not initial
        then fornecedorCnpj.NomeFornecedor
        else fornecedorCpf.NomeFornecedor end as NomeFornecedor,       
        
    torrot.tspid as BpTransportador, 
    transportador.taxnum as CnpjTransportador,   
    transp.name_org1 as Transportador,
    placa1.platenumber as Placa1,
    placa2.platenumber as Placa2,
    placa3.platenumber as Placa3, 
    placa4.platenumber as Placa4,    
    lacre.Total as TotalLacre,
    cor.CorLacre,
    motorista.BpMotorista,
    motorista.Motorista,
    motorista.DocMotorista,
    volume.UnidadeVolume,
    @Semantics.quantity.unitOfMeasure: 'UnidadeVolume'
    volume.Volume                        
}

