FUNCTION zfmfi_desagrupar_fatura.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_BUKRS) TYPE  RF05R-BUKRS
*"     VALUE(IV_AUGBL) TYPE  RF05R-AUGBL
*"     VALUE(IV_GJAHR) TYPE  RF05R-GJAHR
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  CALL FUNCTION 'CALL_FBRA'
    EXPORTING
      i_bukrs      = iv_bukrs
      i_augbl      = iv_augbl
      i_gjahr      = iv_gjahr
    EXCEPTIONS
      not_possible = 1
      OTHERS       = 2.

  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFUNCTION.
