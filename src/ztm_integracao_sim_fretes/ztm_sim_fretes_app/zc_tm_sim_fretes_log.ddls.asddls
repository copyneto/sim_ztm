@EndUserText.label: 'CDS de Consumo - Sim Fretes Log'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_TM_SIM_FRETES_LOG as projection on ZI_TM_SIM_FRETES_LOG
{
    key IdProcesso,
    key IdMsgs,
    Processo,
    Type,
    Id,
    Numero,
    Message,
    Status,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    /* Associations */
    _pai : redirected to parent ZC_TM_SIM_FRETES
}
