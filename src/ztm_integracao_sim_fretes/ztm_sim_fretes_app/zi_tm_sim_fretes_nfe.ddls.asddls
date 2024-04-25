@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface - Sim Fretes NFE'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_TM_SIM_FRETES_NFE as select from zttm_int_fre_nfe as _nfe
association to parent ZI_TM_SIM_FRETES as _pai on _pai.CtcNumero = $projection.CtcNumero
                                              and _pai.CtcSerie = $projection.CtcSerie
{
    key ctc_numero as CtcNumero,
    key ctc_serie as CtcSerie,
    key nf_numero as NfNumero,
    key nf_serie as NfSerie,
    nf_emitente_cnpj as NfEmitenteCnpj,
    nf_chave as NfChave,
    created_by as CreatedBy,
    created_at as CreatedAt,
    last_changed_by as LastChangedBy,
    last_changed_at as LastChangedAt,
    local_last_changed_at as LocalLastChangedAt,
    _pai
}
