@EndUserText.label: 'Custom Entity - Impressão Ordem de Carregamento'
@ObjectModel.query.implementedBy: 'ABAP:ZCLTM_CE_CARREGAMENTO'
define custom entity ZI_TM_CE_ORDEM_CARREGAMENTO
    with parameters p_ordemFrete : char10
{
  key stream_data     : ze_stream_carregamento;
//      db_key          : /bobf/conf_key;
//      OrdemFrete      : /scmtms/tor_id;
//      Transportadora  : /scmtms/pty_carrier;
//      taxnumxl        : bptaxnum;
//      id_processo     : sysuuid_x16;
//      processo        : numc4;
//      last_changed_at : ze_last_changed_at;
//      message         : bapi_msg;
//      status          : ze_status_item;
//      Centro          : /scmtms/plant_id;
//      Placavalo       : /scmtms/resplatenr;
//      Placarreta1     : /scmtms/resplatenr;
//      Placarreta2     : /scmtms/resplatenr;
//      Motorista       : /scmtms/res_name;
//      BR_NotaFiscal   : j_1bdocnum;
//      docdat          : j_1bdocdat;
//      DescMotorista   : /scmtms/item_description;
//      CorLacre        : ze_cor_lacre;

}
