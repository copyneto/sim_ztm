@EndUserText.label: 'CDS de Consumo - Sim Fretes NFE'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_TM_SIM_FRETES_NFE as projection on ZI_TM_SIM_FRETES_NFE
{
    key CtcNumero,
    key CtcSerie,
    key NfNumero,
    key NfSerie,
    NfEmitenteCnpj,
    NfChave,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    _pai : redirected to parent ZC_TM_SIM_FRETES
}
