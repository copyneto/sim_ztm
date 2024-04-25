FUNCTION zfmtm_create_resource_combi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_COMBI_RES_DEF) TYPE  ZCTGTM_COMBINATION_DEF
*"     VALUE(IT_MESSAGE) TYPE  ZCTGTM_LOG_RES_COM
*"  EXPORTING
*"     VALUE(EV_RETURN) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA: lt_combi_res_def TYPE /scmb/cl_tmsres_helper=>tyt_combination_def.

  CLEAR: lt_combi_res_def,
         ev_return.

  lt_combi_res_def = CORRESPONDING #( it_combi_res_def ).

  CHECK lt_combi_res_def IS NOT INITIAL.

  " Deleção de dados existentes
  SELECT tmsresuuid,
         seq_num,
         equi_type,
         equi_code,
         indiv_resuuid
    FROM /scmb/restmscmbr
    INTO TABLE @DATA(lt_combi_res_del)
    FOR ALL ENTRIES IN @lt_combi_res_def
    WHERE tmsresuuid EQ @lt_combi_res_def-tmsresuuid.

  IF sy-subrc IS INITIAL.
    /scmb/cl_tmsres_helper=>save_resource_combi_def( EXPORTING it_combi_res_def = CORRESPONDING #( lt_combi_res_del )
                                                               iv_delete        = abap_true
                                                     IMPORTING ev_saved_entries = DATA(lv_saved_combi_def) ).

    IF lv_saved_combi_def IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
  ENDIF.

  " Criação de recursos combinados
  /scmb/cl_tmsres_helper=>save_resource_combi_def( EXPORTING it_combi_res_def = lt_combi_res_def
                                                   IMPORTING ev_saved_entries = lv_saved_combi_def ).

  zcltm_carga_veiculo=>save_log( it_message = it_message ).

  IF lv_saved_combi_def IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  ev_return = abap_true.

ENDFUNCTION.
