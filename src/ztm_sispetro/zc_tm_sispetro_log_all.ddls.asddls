@EndUserText.label: 'CDS de Consumo - Sispetro Log'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_TM_SISPETRO_LOG_ALL 
as projection on ZI_TM_SISPETRO_LOG_ALL
{
    @EndUserText.label: 'ID Sispetro'
    key id_processo,
    @EndUserText.label: 'ID Mensagem'
    key id_msgs,
    @EndUserText.label: 'Nº Sispetro'
    processo,
    @EndUserText.label: 'Data - Hora - Envio'
    last_changed_at,
    @EndUserText.label: 'Msg. Status'
    message,
    @EndUserText.label: 'Status Envio'
    status,
      /* Associations */
    _pai : redirected to parent ZC_TM_SISPETRO
}
