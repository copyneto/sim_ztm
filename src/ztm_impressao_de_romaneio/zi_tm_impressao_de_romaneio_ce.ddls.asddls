@EndUserText.label: 'CDS -Custon Entity - Impressão Romaneio'
@ObjectModel.query.implementedBy: 'ABAP:ZCLTM_ROMANEIO'
define custom entity ZI_TM_IMPRESSAO_DE_ROMANEIO_CE 
{
    
  
key docnum : j_1bdocnum;
    Nota:    j_1bitmnum;
    Emissao: j_1bdocdat;
    transportadora: name1_gp;
    stream_data  : ze_stream_romaneio;
//    VOL : j_1bcte_vol_transp;
//    Pedido: ebln1;
//    @Semantics.quantity.unitOfMeasure: 'UnidadePeso'
//    Peso : ntgew_15;
//    @Semantics.amount.currencyCode: 'Moeda'
//    Valor:j_1b_billing_org_value;
//    Cliente:j_1bparid;
//    Cidade:ort01_gp;
//    UF:regio;
//    UnidadePeso:gewei;
//    vol_unit:j_1bcte_vol_unit;
//    Moeda:waerk;
  
}
