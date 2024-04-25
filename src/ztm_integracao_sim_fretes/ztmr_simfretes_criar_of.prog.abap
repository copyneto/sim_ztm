***********************************************************************
***                              © OCP                              ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Integração SAP x SIMFRETES - Criar OFE                 *
*** AUTOR : Carlos Adriano Garcia – META                              *
*** FUNCIONAL: Tiago Valada – META                                    *
*** DATA : 14.11.2023                                                 *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES                                        *
***-------------------------------------------------------------------*
*** DATA    | AUTOR         | DESCRIÇÃO                               *
***-------------------------------------------------------------------*
***         |               |                                         *
***********************************************************************

REPORT ztmr_simfretes_criar_of.

* ======================================================================
* Tela de seleção
* ======================================================================

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  PARAMETERS: p_exec TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl1.

* ======================================================================
* Inicio do aplicativo
* ======================================================================

START-OF-SELECTION.
  PERFORM f_start.

* ======================================================================
* Form: F_START
* ======================================================================
* Inicia lógica
* ----------------------------------------------------------------------

FORM f_start.

* ----------------------------------------------------------------------
* Valida campos obrigatórios
* ----------------------------------------------------------------------
  IF p_exec IS INITIAL.
    " Preencher todos os campos obrigatórios
    MESSAGE s055(00) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
* ----------------------------------------------------------------------
* Inicia processo de criação de OF
* ----------------------------------------------------------------------
  DATA(lt_return) = NEW zcltm_integracao_simfretes_of( )->executar( ).

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
