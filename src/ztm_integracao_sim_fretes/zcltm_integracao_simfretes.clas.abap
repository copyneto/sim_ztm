CLASS zcltm_integracao_simfretes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_int_fre_cte   TYPE zttm_int_fre_cte,
      ty_t_int_fre_cte TYPE STANDARD TABLE OF ty_int_fre_cte,
      ty_int_fre_nfe   TYPE zttm_int_fre_nfe,
      ty_t_int_fre_nfe TYPE STANDARD TABLE OF ty_int_fre_nfe,
      ty_decimal       TYPE zttm_int_fre_cte-ctc_valor.

    CONSTANTS:
      BEGIN OF gc_cte_search,
        processo TYPE zi_ca_get_url_cpi_filter-processo VALUE 'ZTM_INTEGRA_SAP_SIMFRETES_CTE',
        metodo   TYPE zi_ca_get_url_cpi_filter-Metodo   VALUE 'POST',
      END OF gc_cte_search,

      BEGIN OF gc_code,
        success             TYPE i VALUE '200', " OK
        created             TYPE i VALUE '201', " Created
        invalid             TYPE i VALUE '400', " Requisição inválida
        not_found           TYPE i VALUE '404', " Entidade não encontrada
        invalid_to_consumer TYPE i VALUE '406', " Recurso não possui representação que poderia ser aceita pelo consumidor
        conflict            TYPE i VALUE '409', " Conflito
        internal_error      TYPE i VALUE '500', " Erro interno do servidor
      END OF gc_code.

    "! Cria instancia
    "! @parameter ro_instance | Instância da classe
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcltm_integracao_simfretes.

    METHODS start_cte_search
      IMPORTING iv_start_date TYPE dats
                iv_start_time TYPE tims OPTIONAL
                iv_end_date   TYPE dats OPTIONAL
                iv_end_time   TYPE tims OPTIONAL
      EXPORTING et_return     TYPE bapiret2_t.

    "! Chama integração CPI
    METHODS call_api_post_cte_search
      IMPORTING is_request       TYPE zstm_integra_simfretes_cte_i
      EXPORTING es_response      TYPE zstm_integra_simfretes_cte_o
                ev_code          TYPE i
                ev_request_json  TYPE string
                ev_response_json TYPE string
                et_return        TYPE bapiret2_t.

    "! Grava dados CTE
    METHODS save_cte
      IMPORTING is_response    TYPE zstm_integra_simfretes_cte_o
      EXPORTING et_int_fre_cte TYPE ty_t_int_fre_cte
                et_int_fre_nfe TYPE ty_t_int_fre_nfe
                et_return      TYPE bapiret2_t.

    "! Converte dados do formato TIMESTAMP para STRING
    METHODS convert_timestamp_to_string
      IMPORTING iv_timestamp   TYPE timestampl
      EXPORTING ev_text        TYPE string
      RETURNING VALUE(rv_text) TYPE string.

    "! Converte dados do formato STRING para TIMESTAMP
    METHODS convert_string_to_timestamp
      IMPORTING iv_text             TYPE string
      EXPORTING ev_timestamp        TYPE timestampl
      RETURNING VALUE(rv_timestamp) TYPE timestampl.

    "! Converte dados do formato STRING para DECIMAL
    METHODS convert_string_to_decimal
      IMPORTING iv_text           TYPE string
      EXPORTING ev_decimal        TYPE ty_decimal
      CHANGING  ct_return         TYPE bapiret2_t OPTIONAL
      RETURNING VALUE(rv_decimal) TYPE ty_decimal.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zcltm_integracao_simfretes.

ENDCLASS.



CLASS zcltm_integracao_simfretes IMPLEMENTATION.

  METHOD get_instance.

    IF ( go_instance IS INITIAL ).
      go_instance = NEW zcltm_integracao_simfretes( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD start_cte_search.

    DATA: ls_request         TYPE zstm_integra_simfretes_cte_i,
          lv_start_timestamp TYPE timestampl,
          lv_end_timestamp   TYPE timestampl.

    FREE: et_return.

* ---------------------------------------------------------------------------
* Inicializa parâmetros de busca
* ---------------------------------------------------------------------------
    DATA(lv_start_date) = COND #( WHEN iv_start_date IS NOT INITIAL
                                  THEN iv_start_date
                                  ELSE '19900101' ).
    DATA(lv_start_time) = iv_start_time.
    DATA(lv_end_date)   = COND #( WHEN iv_end_date IS NOT INITIAL
                                  THEN iv_end_date
                                  ELSE iv_start_date ).
    DATA(lv_end_time)   = COND #( WHEN iv_end_time IS NOT INITIAL
                                  THEN iv_end_time
                                  ELSE '235959' ).

    IF  lv_start_date IS INITIAL AND lv_end_date IS INITIAL.
      " Nenhum filtro informado para iniciar busca.
      et_return = VALUE #( BASE et_return ( type = 'E' id = 'ZTM_INT_SIMFRETES' number = '002' ) ).
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara filtros de busca da integração
* ---------------------------------------------------------------------------
    CONVERT DATE lv_start_date TIME lv_start_time INTO TIME STAMP lv_start_timestamp TIME ZONE 'UTC'.
    CONVERT DATE lv_end_date   TIME lv_end_time   INTO TIME STAMP lv_end_timestamp   TIME ZONE 'UTC'.

    ls_request-ultima_alteracao_de  = me->convert_timestamp_to_string( EXPORTING iv_timestamp = lv_start_timestamp ).
    ls_request-ultima_alteracao_ate = me->convert_timestamp_to_string( EXPORTING iv_timestamp = lv_end_timestamp ).

* ---------------------------------------------------------------------------
* Chama API para consulta dos documentos de transporte CTE
* ---------------------------------------------------------------------------
    me->call_api_post_cte_search( EXPORTING is_request       = ls_request
                                  IMPORTING es_response      = DATA(ls_response)
                                            et_return        = DATA(lt_return) ).

*    IF line_exists( lt_return[ type = 'E' ] ).           "#EC CI_STDSEQ
      INSERT LINES OF lt_return INTO TABLE et_return.
*    ENDIF.

* ---------------------------------------------------------------------------
* Grava os dados
* ---------------------------------------------------------------------------
    me->save_cte( EXPORTING is_response = ls_response
                  IMPORTING et_return   = lt_return  ).

*    IF line_exists( lt_return[ type = 'E' ] ).           "#EC CI_STDSEQ
      INSERT LINES OF lt_return INTO TABLE et_return.
*    ENDIF.

  ENDMETHOD.


  METHOD call_api_post_cte_search.

    FREE: es_response, ev_request_json, ev_response_json, ev_code, et_return.

* ---------------------------------------------------------------------------
* Chama API
* ---------------------------------------------------------------------------
    DATA(lo_cpi) = NEW zclca_cpi( ).

    lo_cpi->send( EXPORTING iv_processo  = gc_cte_search-processo
                            iv_metodo    = gc_cte_search-metodo
                            is_structure = is_request
                  IMPORTING ev_json      = ev_request_json
                            ev_result    = ev_response_json
                            ev_code      = ev_code
                            et_return    = DATA(lt_return) ).

    INSERT LINES OF lt_return INTO TABLE et_return[].

* ---------------------------------------------------------------------------
* Armazena histórico no monitor de chamada CPI
* ---------------------------------------------------------------------------
    DATA(lo_log) = NEW zclca_monitor_cpi( ).

    lo_log->started_process( EXPORTING iv_metodo       = gc_cte_search-metodo
                                       iv_processo     = gc_cte_search-processo
                                       iv_json         = ev_request_json
                             IMPORTING et_return       = DATA(lt_return_log) ).

    lo_log->save_log( EXPORTING iv_processo     = gc_cte_search-processo
                                iv_metodo       = gc_cte_search-metodo
                                iv_json_retorno = ev_response_json
                                iv_json         = ev_request_json
                                it_return       = lt_return
                      IMPORTING et_return       = DATA(lt_return_save) ).

* ---------------------------------------------------------------------------
* Converte .json de retorno
* ---------------------------------------------------------------------------
    lo_cpi->conv_json_to_data( EXPORTING iv_json           = ev_response_json
                               IMPORTING es_structure      = es_response ).

* ---------------------------------------------------------------------------
* Prepara mensagem de sucesso
* ---------------------------------------------------------------------------

    IF ev_code EQ gc_code-success
    OR ev_code EQ gc_code-created.

      IF lines( es_response-data-resultados[] ) > 0.
        " &1 documento(s) de Transporte recuperado(s) com sucesso.
        et_return = VALUE #( BASE et_return ( type = 'S' id = 'ZTM_INT_SIMFRETES' number = '002' message_v1 = |{ CONV string( lines( es_response-data-resultados[] ) ) ALPHA = OUT }| ) ).

      ELSE.

        " Nenhum documento de transporte recuperado para o período informado.
        et_return = VALUE #( BASE et_return ( type = 'E' id = 'ZTM_INT_SIMFRETES' number = '003' ) ).

      ENDIF.

    ELSE.

      IF es_response-data-mensagem IS NOT INITIAL.

        DATA(lv_mensagem) = CONV char200( es_response-data-mensagem ).
        et_return = VALUE #( BASE et_return ( type = 'E' id = 'ZTM_INT_SIMFRETES' number = '000' message_v1 = lv_mensagem+0(50)
                                                                                                 message_v2 = lv_mensagem+50(50)
                                                                                                 message_v3 = lv_mensagem+100(50)
                                                                                                 message_v4 = lv_mensagem+150(50) ) ).

      ELSE.

        " Falha na chamada do serviço SIMFRETES.
        et_return = VALUE #( BASE et_return ( type = 'E' id = 'ZTM_INT_SIMFRETES' number = '004' ) ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD save_cte.

    DATA: lt_timestamp TYPE timestampl,
          lt_return    TYPE bapiret2_t.

    FREE: et_int_fre_cte, et_int_fre_nfe, et_return.

    GET TIME STAMP FIELD lt_timestamp.

    LOOP AT is_response-data-resultados REFERENCE INTO DATA(ls_resultados).

      et_int_fre_cte = VALUE #( BASE et_int_fre_cte (
                                ctc_numero                    = ls_resultados->ctcnumero
                                ctc_serie                     = ls_resultados->ctcserie
                                ctc_emissao                   = me->convert_string_to_timestamp( EXPORTING iv_text = ls_resultados->ctcemissao )
                                ctc_valor                     = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->ctcvalor
                                                                                               CHANGING  ct_return  = lt_return )
                                ctc_chave                     = ls_resultados->ctcchave
                                total_calculado               = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->totalcalculado
                                                                                               CHANGING  ct_return  = lt_return )
                                total_aprovado                = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->totalaprovado
                                                                                               CHANGING  ct_return  = lt_return )
                                base_icms                     = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->baseicms
                                                                                               CHANGING  ct_return  = lt_return )
                                aliquota_icms                 = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->aliquotaicms
                                                                                               CHANGING  ct_return  = lt_return )
                                valor_icms                    = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->valoricms
                                                                                               CHANGING  ct_return  = lt_return )
                                cst_icms                      = me->convert_string_to_decimal( EXPORTING iv_text    = ls_resultados->csticms
                                                                                               CHANGING  ct_return  = lt_return )
                                tipo_frete                    = ls_resultados->tipofrete
                                modal                         = ls_resultados->modal
                                tipo_operacao                 = ls_resultados->tipooperacao
                                observacoes                   = ls_resultados->observacoes
                                fornecedor_cnpj               = ls_resultados->fornecedorcnpj
                                fornecedor_nome               = ls_resultados->fornecedornome
                                fornecedor_ie                 = ls_resultados->fornecedorie
                                remetente_cnpj                = ls_resultados->remetentecnpj
                                remetente_nome                = ls_resultados->remetentenome
                                destinatario_cnpj             = ls_resultados->destinatariocnpj
                                destinatario_nome             = ls_resultados->destinatarionome
                                recebedor_cnpj                = ls_resultados->recebedorcnpj
                                recebedor_nome                = ls_resultados->recebedornome
                                origem_localidade             = ls_resultados->origemlocalidade
                                origem_uf                     = ls_resultados->origemuf
                                origem_ibge                   = ls_resultados->origemibge
                                destino_localidade            = ls_resultados->destinolocalidade
                                destino_uf                    = ls_resultados->destinouf
                                destino_ibge                  = ls_resultados->destinoibge
                                codigo_imposto_erp            = ls_resultados->codigoimpostoerp
                                documento_transporte_situacao = ls_resultados->documentotransportesituacao
                                data_hora_situacao            = me->convert_string_to_timestamp( EXPORTING iv_text = ls_resultados->datahorasituacao )
                                frete_especial                = ls_resultados->freteespecial
                                cfop                          = ls_resultados->cfop
                                cfop_original                 = ls_resultados->cfoporiginal
                                chave_cte_origem              = ls_resultados->chavecteorigem
                                xml                           = ls_resultados->xml
                                created_by                    = sy-uname
                                created_at                    = lt_timestamp
                                local_last_changed_at         = lt_timestamp
                                ) ).

      et_int_fre_nfe = VALUE #( BASE et_int_fre_nfe FOR ls_nf_ IN ls_resultados->notasfiscais (
                                ctc_numero                    = ls_resultados->ctcnumero
                                ctc_serie                     = ls_resultados->ctcserie
                                nf_numero                     = ls_nf_-nfnumero
                                nf_serie                      = ls_nf_-nfserie
                                nf_emitente_cnpj              = ls_nf_-nfemitentecnpj
                                nf_chave                      = ls_nf_-nfchave
                                created_by                    = sy-uname
                                created_at                    = lt_timestamp
                                local_last_changed_at         = lt_timestamp
                                ) ).

    ENDLOOP.

* ---------------------------------------------------------------------------
* Grava dados CTE
* ---------------------------------------------------------------------------
    IF et_int_fre_cte[] IS NOT INITIAL.

      " Grava dados CTE
      MODIFY zttm_int_fre_cte FROM TABLE et_int_fre_cte[].

      IF sy-subrc EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ENDIF.

* ---------------------------------------------------------------------------
* Grava dados CTE x NFE
* ---------------------------------------------------------------------------
    IF et_int_fre_nfe[] IS NOT INITIAL.

      MODIFY zttm_int_fre_nfe FROM TABLE et_int_fre_nfe[].

      IF sy-subrc EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD convert_timestamp_to_string.

    FREE: ev_text, rv_text.

    DATA(lv_conv) = CONV char40( iv_timestamp ).
    lv_conv = replace( val = lv_conv pcre = '[^0-9]' with = '' occ = 0 ).

    rv_text =  ev_text = |{ CONV numc04( lv_conv+0(4) ) }-{ CONV monat( lv_conv+4(2) ) }-{ CONV monat( lv_conv+6(2) ) }T{ CONV monat( lv_conv+8(2) ) }:{ CONV monat( lv_conv+10(2) ) }:{ CONV monat( lv_conv+12(2) ) }|.

  ENDMETHOD.


  METHOD convert_string_to_timestamp.

    FREE: ev_timestamp, rv_timestamp.

    DATA(lv_conv) = CONV char40( iv_text ).
    lv_conv = replace( val = lv_conv pcre = '[^0-9]' with = '' occ = 0 ).

    rv_timestamp =  ev_timestamp = |{ lv_conv+0(14) }|.

  ENDMETHOD.


  METHOD convert_string_to_decimal.

    FREE: ev_decimal, rv_decimal.

    TRY.
        rv_decimal = ev_decimal = iv_text.

      CATCH cx_root INTO DATA(lo_root).

        " Falha ao converter texto ' &1' para valor.
        ct_return[] = VALUE #( BASE ct_return ( type = 'E' id = 'ZTM_INT_SIMFRETES' number = '005' message_v1 = iv_text ) ).

    ENDTRY.

  ENDMETHOD.


ENDCLASS.
