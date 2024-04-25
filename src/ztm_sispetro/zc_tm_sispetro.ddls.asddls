@EndUserText.label: 'CDS de Consumo - Sispetro'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_TM_SISPETRO
  provider contract transactional_query
  as projection on ZI_TM_SISPETRO
{
  key db_key,
      @EndUserText.label: 'Ordem Frete'
      @Consumption.semanticObject: 'FreightOrder'
      OrdemFrete,
      @EndUserText.label: 'Transportadora'
      Transportadora,
      @EndUserText.label: 'CNPJ Transportadora'
      taxnumxl,
      //    @EndUserText.label: 'Centro'
      //    Centro,
      //    @EndUserText.label: 'Placa Cavalo'
      //    Placavalo,
      //    @EndUserText.label: 'Placa Carreta 1'
      //    Placarreta1,
      //    @EndUserText.label: 'Placa Carreta 2'
      //    Placarreta2,
      //    @EndUserText.label: 'Nota Fiscal'
      //    BR_NotaFiscal,
      //    @EndUserText.label: 'Data NF'
      //    docdat,
      @EndUserText.label: 'ID Sispetro'
      id_processo,
      @EndUserText.label: 'Nº Sispetro'
      processo,
      @EndUserText.label: 'Nº Sispetro'
      sispetro,
      @EndUserText.label: 'Data - Hora - Envio'
      last_changed_at,
      @EndUserText.label: 'Msg. Status'
      message,
      @EndUserText.label: 'Status Envio'
      status,
      @EndUserText.label: 'Centro'
      Centro,
      @EndUserText.label: 'Placa Cavalo'
      Placavalo,
      @EndUserText.label: 'Placa Carreta 1'
      Placarreta1,
      @EndUserText.label: 'Placa Carreta 2'
      Placarreta2,
      @EndUserText.label: 'Motorista'
      Motorista,
      @EndUserText.label: 'Descr. Motorista'
      DescMotorista,
      @EndUserText.label: 'Cor Lacre'
      CorLacre,
      @EndUserText.label: 'Unidade Volume'
      gro_vol_uni,
      @EndUserText.label: 'Volume Total'
      VolumeTotal,
      @EndUserText.label: 'Ordem Frete'
      @Consumption.semanticObject: 'FreightOrder'
      FreightOrder,
      @EndUserText.label: 'Status Ordem'
      @ObjectModel.text.element: [ 'TranspOrdLifeCycleStatusDesc' ]
      TranspOrdLifeCycleStatus,
      TranspOrdLifeCycleStatusDesc,
      //    @EndUserText.label: 'Motorista'
      //    Motorista,
      //    @EndUserText.label: 'Descr. Motorista'
      //    DescMotorista,
      /* Associations */
      _loga           : redirected to composition child ZC_TM_SISPETRO_LOG_ALL,
      _NotaFiscal     : redirected to composition child ZC_TM_SISPETRO_NOTA_FISCAL,
      _Compartimentos : redirected to composition child ZC_TM_ORDEM_CARREGAMENTO_COMP,
      _Lacres         : redirected to composition child ZC_TM_ORDEM_CARREG_LISTA_LACRE


}
