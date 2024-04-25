FUNCTION zfmfi_agrupar_fatura.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_CTCNUMERO) TYPE  NUMC10 OPTIONAL
*"     VALUE(IV_CTCSERIE) TYPE  NUMC4 OPTIONAL
*"     VALUE(IV_DATABASEPOPUP) TYPE  DZFBDT OPTIONAL
*"     VALUE(IV_CONDPGTOPOPUP) TYPE  DZTERM OPTIONAL
*"     VALUE(IV_FORMAPGTOPOPUP) TYPE  SCHZW_BSEG OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  et_return = NEW zclfi_sim_fretes_agrup_faturas( )->executar(
    iv_ctcnumero      = iv_ctcnumero
    iv_ctcserie       = iv_ctcserie
    iv_databasepopup  = iv_databasepopup
    iv_condpgtopopup  = iv_condpgtopopup
    iv_formapgtopopup = iv_formapgtopopup
  ).

ENDFUNCTION.
