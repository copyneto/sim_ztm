***********************************************************************
***                              © OCP                              ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Integração SAP x SIMFRETES - Dados CTE                 *
*** AUTOR : Jong Wan Silva – META                                     *
*** FUNCIONAL: Tiago Valada – META                                    *
*** DATA : 10.11.2023                                                 *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES                                        *
***-------------------------------------------------------------------*
*** DATA    | AUTOR         | DESCRIÇÃO                               *
***-------------------------------------------------------------------*
***         |               |                                         *
***********************************************************************

REPORT ztmr_integracao_simfretes_cte.

* ======================================================================
* Tabelas
* ======================================================================

TABLES: j_1bnfdoc.

* ======================================================================
* Tela de seleção
* ======================================================================

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  SELECT-OPTIONS: s_dtemi FOR j_1bnfdoc-pstdat NO-EXTENSION,
                  s_hremi FOR J_1bnfdoc-hemi NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK bl1.

* ======================================================================
* Inicialização
* ======================================================================

INITIALIZATION.

  PERFORM f_initialize_filter.

* ======================================================================
* Inicio do aplicativo
* ======================================================================

START-OF-SELECTION.
  PERFORM f_start.

* ======================================================================
* Form: F_INITIALIZE_FILTER
* ======================================================================
* Inicializa filtros de seleção
* ----------------------------------------------------------------------

FORM f_initialize_filter.

* ----------------------------------------------------------------------
* Recupera a data/hora do último registro consultado
* ----------------------------------------------------------------------
  SELECT MAX( ctc_emissao )
      FROM zttm_int_fre_cte
      WHERE ctc_emissao IS NOT INITIAL
      INTO @DATA(lv_ctc_emissao).

  IF sy-subrc NE 0.
    CLEAR lv_ctc_emissao.
  ENDIF.

* ----------------------------------------------------------------------
* Prepara intervalo
* ----------------------------------------------------------------------
  CONVERT TIME STAMP lv_ctc_emissao TIME ZONE 'UTC'
         INTO DATE DATA(lv_start_date) TIME DATA(lv_start_time).

  s_dtemi[] = VALUE #( ( sign     = 'I'
                         option   = 'BT'
                         low      = lv_start_date
                         high     = sy-datum ) ).

  s_hremi[] = VALUE #( ( sign     = 'I'
                         option   = 'BT'
                         low      = lv_start_time
                         high     = sy-uzeit ) ).

ENDFORM.

* ======================================================================
* Form: F_START
* ======================================================================
* Inicia lógica
* ----------------------------------------------------------------------

FORM f_start.

* ----------------------------------------------------------------------
* Valida campos obrigatórios
* ----------------------------------------------------------------------
  IF s_dtemi[] IS INITIAL.
    " Preencher todos os campos obrigatórios
    MESSAGE s055(00) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  TRY.
      DATA(lv_start_date) = s_dtemi[ 1 ]-low.
      DATA(lv_end_date)   = s_dtemi[ 1 ]-high.
    CATCH cx_root.
  ENDTRY.

  TRY.
      DATA(lv_start_time) = s_hremi[ 1 ]-low.
      DATA(lv_end_time)   = s_hremi[ 1 ]-high.
    CATCH cx_root.
  ENDTRY.

* ----------------------------------------------------------------------
* Inicia processo de integração de dados CTE
* ----------------------------------------------------------------------
  DATA(lo_integracao) = zcltm_integracao_simfretes=>get_instance( ).

  lo_integracao->start_cte_search( EXPORTING iv_start_date = lv_start_date
                                             iv_start_time = lv_start_time
                                             iv_end_date   = lv_end_date
                                             iv_end_time   = lv_end_time
                                   IMPORTING et_return     = DATA(lt_return) ).

* ----------------------------------------------------------------------
* Exibe mensagens de retorno
* ----------------------------------------------------------------------
  PERFORM f_show_log USING lt_return.

ENDFORM.

* ======================================================================
* Form: F_SHOW_LOG
* ======================================================================
*Exibe log
* ----------------------------------------------------------------------

FORM f_show_log USING ut_return TYPE bapiret2_t.

  DATA: lt_message TYPE esp1_message_tab_type.

  CHECK ut_return[] IS NOT INITIAL.

* ----------------------------------------------------------------------
* Prepara as mensagens
* ----------------------------------------------------------------------

  LOOP AT ut_return REFERENCE INTO DATA(ls_return).

    lt_message = VALUE #( BASE lt_message ( msgid  = ls_return->id
                                            msgty  = ls_return->type
                                            msgno  = ls_return->number
                                            msgv1  = ls_return->message_v1
                                            msgv2  = ls_return->message_v2
                                            msgv3  = ls_return->message_v3
                                            msgv4  = ls_return->message_v4
                                            lineno = sy-tabix ) ).
  ENDLOOP.

* ----------------------------------------------------------------------
* Exibe apenas uma mensagem
* ----------------------------------------------------------------------
  IF lines( lt_message[] ) = 1.

    READ TABLE lt_message INTO DATA(ls_message) INDEX 1.

    MESSAGE ID ls_message-msgid TYPE 'S' NUMBER ls_message-msgno
            DISPLAY LIKE ls_message-msgty
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.

* ----------------------------------------------------------------------
* Exibe múltiplas mensagens como pop-up
* ----------------------------------------------------------------------
  ELSE.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_message[].

  ENDIF.

ENDFORM.
