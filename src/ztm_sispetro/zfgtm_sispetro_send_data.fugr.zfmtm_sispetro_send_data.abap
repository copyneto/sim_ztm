FUNCTION zfmtm_sispetro_send_data.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_HEADER) TYPE  STRING
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"     VALUE(EV_RETURN) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: ls_header  TYPE zcltm_sispetro_send_data=>ty_header,
        ls_num_oc  TYPE zcltm_sispetro_send_data=>ty_num_oc,
        ls_num_oc1 TYPE zcltm_sispetro_send_data=>ty_num_oc_data1.

  DATA(lo_cpi)         = NEW zclca_cpi( ).
  DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).

  /ui2/cl_json=>deserialize( EXPORTING json = iv_header
                             CHANGING  data = ls_header ).

  lo_cpi->send( EXPORTING iv_processo  = zcltm_sispetro_send_data=>gc_cpi-processo
                          iv_metodo    = zcltm_sispetro_send_data=>gc_cpi-method_post
                          is_structure = ls_header
                IMPORTING ev_result    = ev_return
                          et_return    = DATA(lt_return) ).

  /ui2/cl_json=>deserialize( EXPORTING json = ev_return
                           CHANGING  data = ls_num_oc1 ).

  IF ( lt_return IS NOT INITIAL ).
    APPEND VALUE #( id         = zcltm_sispetro_send_data=>gc_message-id
                    type       = if_xo_const_message=>error
                    number     = zcltm_sispetro_send_data=>gc_message-number_001
                    message_v1 = ls_header-ordem_carregamento-webNumeroOCCliente
                    message_v2 = ls_num_oc1-data-numerooc ) TO et_return.
  ELSE.
    APPEND VALUE #( id         = zcltm_sispetro_send_data=>gc_message-id
                    type       = if_xo_const_message=>success
                    number     = zcltm_sispetro_send_data=>gc_message-number_002
                    message_v1 = ls_header-ordem_carregamento-webNumeroOCCliente
                    message_v2 = ls_num_oc1-data-numerooc ) TO et_return.
  ENDIF.

  lo_cpi_monitor->started_process( EXPORTING iv_processo  = zcltm_sispetro_send_data=>gc_cpi-processo
                                             iv_metodo    = zcltm_sispetro_send_data=>gc_cpi-method_post
                                             iv_chave_ref = CONV #( ls_header-ordem_carregamento-webNumeroOCCliente )
                                             iv_json      = iv_header ).

  lo_cpi_monitor->save_log( EXPORTING iv_processo     = zcltm_sispetro_send_data=>gc_cpi-processo
                                      iv_metodo       = zcltm_sispetro_send_data=>gc_cpi-method_post
                                      iv_json_retorno = ev_return
                                      iv_json         = iv_header
                                      it_return       = et_return ).


  /ui2/cl_json=>deserialize( EXPORTING json = ev_return
                             CHANGING  data = ls_num_oc ).

  /ui2/cl_json=>deserialize( EXPORTING json = ev_return
                             CHANGING  data = ls_num_oc1 ).


  LOOP AT ls_num_oc1-errormessages ASSIGNING FIELD-SYMBOL(<fs_errormessages>).
    APPEND VALUE #( id         = ''
                    type       = if_xo_const_message=>error
                    number     = ''
                    message    = <fs_errormessages> ) TO et_return.
  ENDLOOP.

  IF ls_num_oc1-data-numerooc IS NOT INITIAL.
    zcltm_sispetro_send_data=>update_tor_root( iv_tor_id = CONV #( ls_header-ordem_carregamento-webNumeroOCCliente )
                                               iv_num_oc = ls_num_oc1-data-numerooc ).
  ENDIF.

ENDFUNCTION.
