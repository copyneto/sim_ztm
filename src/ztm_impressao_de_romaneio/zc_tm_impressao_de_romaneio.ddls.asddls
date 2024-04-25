@EndUserText.label: 'CDS - Impressão de Romaneio'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_TM_IMPRESSAO_DE_ROMANEIO as projection on ZI_TM_IMPRESSAO_DE_ROMANEIO {
    key docnum,
    Nota,
    Emissao,
    VOL,
    Pedido,
    Peso,
    Valor,
    Cliente,
    Cidade,
    UF,
    UnidadePeso,
    vol_unit,
    Moeda,
    @EndUserText.label: 'Transportadora'
    @ObjectModel.text.element: [ 'TransportadoraDesc' ]
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_TM_VH_TRANSPORTADORA', element: 'Transportadora'} }]
    Transportadora,
    @EndUserText.label: 'Nome Transportadora'
    TransportadoraDesc
}
