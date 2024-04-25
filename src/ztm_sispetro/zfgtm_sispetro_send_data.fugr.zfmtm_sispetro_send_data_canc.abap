FUNCTION zfmtm_sispetro_send_data_canc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_CANCEL) TYPE  STRING
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"     VALUE(EV_RETURN) TYPE  STRING
*"----------------------------------------------------------------------

  DATA ls_cancel TYPE zcltm_sispetro_send_data=>ty_cancel.

  DATA(lo_cpi)         = NEW zclca_cpi( ).
  DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).

  /ui2/cl_json=>deserialize( EXPORTING json = iv_cancel
                             CHANGING  data = ls_cancel ).

  CALL METHOD lo_cpi->send
    EXPORTING
      iv_processo  = zcltm_sispetro_send_data=>gc_cpi_cancel-processo
      iv_metodo    = zcltm_sispetro_send_data=>gc_cpi_cancel-method_post
      is_structure = ls_cancel
    IMPORTING
      ev_result    = ev_return
      et_return    = DATA(lt_return).

  IF ( lt_return IS NOT INITIAL ).
    APPEND VALUE #( id         = zcltm_sispetro_send_data=>gc_message-id
                    type       = if_xo_const_message=>error
                    number     = zcltm_sispetro_send_data=>gc_message-number_003
                    message_v1 = ls_cancel-id ) TO et_return.
  ELSE.
    APPEND VALUE #( id         = zcltm_sispetro_send_data=>gc_message-id
                    type       = if_xo_const_message=>success
                    number     = zcltm_sispetro_send_data=>gc_message-number_004
                    message_v1 = ls_cancel-id ) TO et_return.
  ENDIF.

  lo_cpi_monitor->started_process( EXPORTING iv_processo  = zcltm_sispetro_send_data=>gc_cpi_cancel-processo
                                             iv_metodo    = zcltm_sispetro_send_data=>gc_cpi_cancel-method_post
                                             iv_chave_ref = CONV #( ls_cancel-id )
                                             iv_json      = iv_cancel ).

  lo_cpi_monitor->save_log( EXPORTING iv_processo     = zcltm_sispetro_send_data=>gc_cpi_cancel-processo
                                      iv_metodo       = zcltm_sispetro_send_data=>gc_cpi_cancel-method_post
                                      iv_json_retorno = ev_return
                                      iv_json         = iv_cancel
                                      it_return       = et_return ).

ENDFUNCTION.
