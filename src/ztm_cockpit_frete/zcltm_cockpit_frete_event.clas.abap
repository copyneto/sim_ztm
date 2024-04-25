***********************************************************************
***                             © REDE SIM                          ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Cockpit de Transporte - Botões - Lógica                *
*** AUTOR : Jong Silva – Meta                                         *
*** FUNCIONAL: Marcos Pereira – Meta                                  *
*** DATA : 08.12.2023                                                 *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES                                        *
***-------------------------------------------------------------------*
*** DATA | AUTOR | DESCRIÇÃO                                          *
***-------------------------------------------------------------------*
*** | |                                                               *
***********************************************************************

CLASS zcltm_cockpit_frete_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_prepare,
        controller          TYPE REF TO /bofu/if_fbi_controller_new,
        log                 TYPE REF TO /scmtms/cl_ui_log_pln,
        mp_tc               TYPE REF TO /scmtms/if_pln_tbo_excfunc,
        tbo_srvmgr          TYPE REF TO /bobf/if_tra_service_manager,
        tor_srvmgr          TYPE REF TO /bobf/if_tra_service_manager,
        block_mw_processing TYPE boole_d,
      END OF ty_prepare,

      ty_r_bukrs TYPE RANGE OF bukrs.

    CONSTANTS:
      BEGIN OF gc_action,
        new_tor          TYPE fpm_event_id VALUE 'ZTM_AID_AVR_LIST_NEW_TOR' ##NO_TEXT,
        tor_transf       TYPE fpm_event_id VALUE 'ZTM_AID_AVR_LIST_NEW_TOR_TRSNF' ##NO_TEXT,
        saida_mercadoria TYPE fpm_event_id VALUE 'ZTM_AID_TOR_SAIDA_MERCADORIA' ##NO_TEXT,
        old_tor          TYPE fpm_event_id VALUE 'AID_AVR_LIST_NEW_TOR' ##NO_TEXT,
        aloc_manual      TYPE fpm_event_id VALUE 'ZTB_ALOC_MANUAL_ACTION' ##NO_TEXT,
        close_dialog     TYPE fpm_event_id VALUE 'FPM_CLOSE_DIALOG',
      END OF gc_action,

      BEGIN OF gc_memory,
        aloc_manual TYPE abapdocu_buff_srtfd VALUE 'ZTM_EQUI_INDEX' ##NO_TEXT,
      END OF gc_memory,

      BEGIN OF gc_equi_fields,
        zz_fu_db_key           TYPE string VALUE 'ZZ_FU_DB_KEY' ##NO_TEXT,
        zz_fu_tor_id           TYPE string VALUE 'ZZ_FU_TOR_ID' ##NO_TEXT,
        zz_fu_max_util         TYPE string VALUE 'ZZ_FU_MAX_UTIL' ##NO_TEXT,
        zz_seal_number_total   TYPE string VALUE 'ZZ_SEAL_NUMBER_TOTAL' ##NO_TEXT,
        zz_seal_number_        TYPE string VALUE 'ZZ_SEAL_NUMBER_' ##NO_TEXT,
        zz_seal_number_1       TYPE string VALUE 'ZZ_SEAL_NUMBER_1' ##NO_TEXT,
        zz_seal_number_2       TYPE string VALUE 'ZZ_SEAL_NUMBER_2' ##NO_TEXT,
        zz_seal_number_3       TYPE string VALUE 'ZZ_SEAL_NUMBER_3' ##NO_TEXT,
        zz_seal_number_4       TYPE string VALUE 'ZZ_SEAL_NUMBER_4' ##NO_TEXT,
        zz_seal_number_5       TYPE string VALUE 'ZZ_SEAL_NUMBER_5' ##NO_TEXT,
        zz_sample_envelope     TYPE string VALUE 'ZZ_SAMPLE_ENVELOPE' ##NO_TEXT,
        max_seal_number        TYPE i VALUE 12,
        max_seal_number_perfil TYPE i VALUE 8,
      END OF gc_equi_fields,

      BEGIN OF gc_seal_fields,
        zz_fu_erp_plant_id TYPE string VALUE 'ZZ_FU_ERP_PLANT_ID' ##NO_TEXT,
      END OF gc_seal_fields,

      BEGIN OF gc_param_criar_tipo_doc,
        modulo TYPE  ztca_param_mod-modulo VALUE 'TM',
        chave1 TYPE  ztca_param_par-chave1 VALUE 'COCKPIT_TRANSPORTE',
        chave2 TYPE  ztca_param_par-chave2 VALUE 'BOTAO_CRIA_ORDEM_FRETE',
        chave3 TYPE  ztca_param_par-chave3 VALUE 'TIPO_DOC',
        vendas TYPE ztca_param_val-low     VALUE 'VENDAS',
        transf TYPE ztca_param_val-low     VALUE 'TRANSFERENCIA',
      END OF gc_param_criar_tipo_doc,

      BEGIN OF gc_param_saida_empresa,
        modulo             TYPE  ztca_param_mod-modulo VALUE 'TM',
        chave1             TYPE  ztca_param_par-chave1 VALUE 'COCKPIT_TRANSPORTE',
        chave2             TYPE  ztca_param_par-chave2 VALUE 'BOTAO_SAIDA_MERCADORIA',
        chave3             TYPE  ztca_param_par-chave3 VALUE 'EMPRESA',
        validacao_completa TYPE ztca_param_val-low     VALUE 'VALIDACAO_COMPLETA',
        validacao_parcial  TYPE ztca_param_val-low     VALUE 'VALIDACAO_PARCIAL',
      END OF gc_param_saida_empresa,

      BEGIN OF gc_message,
        id TYPE sy-msgid VALUE 'ZTM_COCKPIT_FRETE',
      END OF gc_message,

      BEGIN OF gc_range,
        include TYPE string VALUE 'I',
        equal   TYPE string VALUE 'EQ',
      END OF gc_range,

      BEGIN OF gc_item_type,
        truc TYPE /scmtms/d_torite-item_type VALUE 'TRUC',
        prd  TYPE /scmtms/d_torite-item_type VALUE 'PRD',
      END OF gc_item_type,

      BEGIN OF gc_mtr,
        trk  TYPE /scmtms/d_torite-mtr VALUE 'ZMTR-TRK',
        conj TYPE /scmtms/d_torite-mtr VALUE 'ZMTR-CONJ',
        cav  TYPE /scmtms/d_torite-mtr VALUE 'ZMTR-CAV',
        carr TYPE /scmtms/d_torite-mtr VALUE 'ZMTR-CARR',
      END OF gc_mtr,

      BEGIN OF gc_saida_merc_event,
        actual_tzone    TYPE /scmtms/tzone            VALUE 'BRAZIL',
        event_code      TYPE /scmtms/tor_event        VALUE 'Z_EXEC_SAIDA_MERC',
        event_status    TYPE /scmtms/tor_event_status VALUE 'N',   " Evento inesperado
        trans_activity  TYPE /scmtms/transp_act       VALUE '02',  " Descarregamento
        execinfo_source TYPE /scmtms/execinfo_source  VALUE 'A',   " Gravado diretamente neste documento
      END OF gc_saida_merc_event,

      BEGIN OF gc_constants,
        fu_root_key_0 TYPE /scmtms/tor_key VALUE '00000000000000000000000000000000',
      END OF gc_constants,

      BEGIN OF gc_dlv_goods_mvmnt_sts,
        not_relevant         TYPE /scmtms/dlv_goods_movem_status VALUE ' ', " Não relevante
        not_processed        TYPE /scmtms/dlv_goods_movem_status VALUE 'A', " Ainda não processado
        partially_processed  TYPE /scmtms/dlv_goods_movem_status VALUE 'B', " Parcialmente processado
        completely_processed TYPE /scmtms/dlv_goods_movem_status VALUE 'C', " Completamente processado
      END OF gc_dlv_goods_mvmnt_sts,

      BEGIN OF gc_load_plan_sts,
        not_planned  TYPE /scmtms/tor_i_load_plan_sts  VALUE '00',  " Não planejado
        not_relevant TYPE /scmtms/tor_i_load_plan_sts  VALUE '05',  " Não relevante
        invalid      TYPE /scmtms/tor_i_load_plan_sts  VALUE '10',  " Invalidado
        planned      TYPE /scmtms/tor_i_load_plan_sts  VALUE '15',  " Planejado
        complete     TYPE /scmtms/tor_i_load_plan_sts  VALUE '20',  " Finalizado
      END OF gc_load_plan_sts.

    "! Cria instancia
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zcltm_cockpit_frete_event.

    "! Exit chamada na classe post-method /SCMTMS/CL_UI_VIEWEXIT_PLN
    METHODS exit_adapt_event
      IMPORTING
        iv_bo_key             TYPE /bobf/obm_bo_key
        iv_node_key           TYPE /bobf/obm_node_key
        iv_eventid            TYPE string
        ir_event_list_line    TYPE REF TO data OPTIONAL
        ir_event_data         TYPE data OPTIONAL
        it_selected_rows      TYPE /bofu/if_fbi_view_instance_new=>tt_row OPTIONAL
        io_additional_info    TYPE REF TO object OPTIONAL
        iv_raised_by_own_ui   TYPE boole_d OPTIONAL
        is_ui_context_data    TYPE /bofu/if_fbi_view_instance_new=>ts_ui_context_data OPTIONAL
        io_ui_info            TYPE REF TO if_fpm_list_ats_ui_info OPTIONAL
      CHANGING
        cv_failed             TYPE boole_d
        cv_event_result_defer TYPE boole_d OPTIONAL.

    "! Prepara os dados para chamar outro botão standard
    METHODS prepare_old_tor
      IMPORTING
        io_controller          TYPE REF TO /bofu/if_fbi_controller_new
        io_log                 TYPE REF TO /scmtms/cl_ui_log_pln
        io_mp_tc               TYPE REF TO /scmtms/if_pln_tbo_excfunc
        io_tbo_srvmgr          TYPE REF TO /bobf/if_tra_service_manager
        io_tor_srvmgr          TYPE REF TO /bobf/if_tra_service_manager
        io_event               TYPE REF TO cl_fpm_event
        iv_block_mw_processing TYPE boole_d OPTIONAL
      EXPORTING
        ev_result              TYPE fpm_event_result
        et_messages            TYPE fpmgb_t_messages.

    "! Prepara os dados para executar outro botão standard
    METHODS execute_old_tor
      IMPORTING
        io_event                   TYPE REF TO cl_fpm_event
      EXPORTING
        ev_suppress_dirty_handling TYPE boole_d
        ev_trigger_save            TYPE boole_d
        ev_trigger_refresh         TYPE boole_d
        ev_trigger_deselect        TYPE boole_d
        ev_result                  TYPE fpm_event_result
        et_messages                TYPE fpmgb_t_messages.

    "! Verifica se as linhas foram selecionadas
    METHODS check_selection_new_tor_fob
      IMPORTING
        io_event     TYPE REF TO cl_fpm_event
        is_selection TYPE /scmtms/cl_ui_controller_tcsel=>ty_selection
      EXPORTING
        ev_result    TYPE fpm_event_result
        et_messages  TYPE fpmgb_t_messages.

    "! Validação para botão Saída de Mercadoria
    METHODS check_selection_saida_merc
      IMPORTING
        io_event     TYPE REF TO cl_fpm_event
        is_selection TYPE /scmtms/cl_ui_controller_tcsel=>ty_selection
      EXPORTING
        ev_result    TYPE fpm_event_result
        et_messages  TYPE fpmgb_t_messages.

    "! Validação para botão Saída de Mercadoria - empresa SIM
    METHODS check_selection_sm_complete
      IMPORTING
        ir_bukrs      TYPE ty_r_bukrs
        it_root       TYPE /scmtms/t_tor_root_k
        it_item_tr    TYPE /scmtms/t_tor_item_tr_k
        it_seal       TYPE /scmtms/t_tor_seal_k
        it_child_item TYPE /scmtms/t_tor_item_tr_k
        it_fu_root    TYPE /scmtms/t_tor_root_k
      EXPORTING
        ev_result     TYPE fpm_event_result
        et_messages   TYPE fpmgb_t_messages.

    "! Validação para botão Saída de Mercadoria - empresa DISTRIBUIDORA CHARRUA
    METHODS check_selection_sm_partial
      IMPORTING
        ir_bukrs      TYPE ty_r_bukrs
        it_root       TYPE /scmtms/t_tor_root_k
        it_item_tr    TYPE /scmtms/t_tor_item_tr_k
        it_seal       TYPE /scmtms/t_tor_seal_k
        it_child_item TYPE /scmtms/t_tor_item_tr_k
        it_fu_root    TYPE /scmtms/t_tor_root_k
      EXPORTING
        ev_result     TYPE fpm_event_result
        et_messages   TYPE fpmgb_t_messages.

    "! Prepara os dados para evento do botão ZTM_AID_AVR_LIST_NEW_TOR
    METHODS prepare_new_tor_fob
      IMPORTING
        io_controller          TYPE REF TO /bofu/if_fbi_controller_new
        io_log                 TYPE REF TO /scmtms/cl_ui_log_pln
        io_mp_tc               TYPE REF TO /scmtms/if_pln_tbo_excfunc
        io_tbo_srvmgr          TYPE REF TO /bobf/if_tra_service_manager
        io_tor_srvmgr          TYPE REF TO /bobf/if_tra_service_manager
        io_event               TYPE REF TO cl_fpm_event
        iv_block_mw_processing TYPE boole_d OPTIONAL
      EXPORTING
        ev_result              TYPE fpm_event_result
        et_messages            TYPE fpmgb_t_messages.

    "! Prepara os dados para evento do botão ZTM_AID_TOR_SAIDA_MERCADORIA
    METHODS prepare_saida_merc
      IMPORTING
        io_controller          TYPE REF TO /bofu/if_fbi_controller_new
        io_log                 TYPE REF TO /scmtms/cl_ui_log_pln
        io_mp_tc               TYPE REF TO /scmtms/if_pln_tbo_excfunc
        io_tbo_srvmgr          TYPE REF TO /bobf/if_tra_service_manager
        io_tor_srvmgr          TYPE REF TO /bobf/if_tra_service_manager
        io_event               TYPE REF TO cl_fpm_event
        iv_block_mw_processing TYPE boole_d OPTIONAL
      EXPORTING
        ev_result              TYPE fpm_event_result
        et_messages            TYPE fpmgb_t_messages.

    "! Prepara os dados para executar o evento do botão ZTM_AID_AVR_LIST_NEW_TOR
    METHODS execute_new_tor_fob
      IMPORTING
        io_event                   TYPE REF TO cl_fpm_event
        is_selection               TYPE /scmtms/cl_ui_controller_tcsel=>ty_selection
      EXPORTING
        ev_suppress_dirty_handling TYPE boole_d
        ev_trigger_save            TYPE boole_d
        ev_trigger_refresh         TYPE boole_d
        ev_trigger_deselect        TYPE boole_d
        ev_result                  TYPE fpm_event_result
        et_messages                TYPE fpmgb_t_messages.

    "! Prepara os dados para executar o evento do botão ZTM_AID_TOR_SAIDA_MERCADORIA
    METHODS execute_saida_merc
      IMPORTING
        io_event                   TYPE REF TO cl_fpm_event
        is_selection               TYPE /scmtms/cl_ui_controller_tcsel=>ty_selection
      EXPORTING
        ev_suppress_dirty_handling TYPE boole_d
        ev_trigger_save            TYPE boole_d
        ev_trigger_refresh         TYPE boole_d
        ev_trigger_deselect        TYPE boole_d
        ev_result                  TYPE fpm_event_result
        et_messages                TYPE fpmgb_t_messages.

    "! Atualiza informações de equipamento da ordem de frete
    METHODS execute_update_fo_equi_info
      IMPORTING is_ctx        TYPE /bobf/s_frw_ctx_det
                it_key        TYPE /bobf/t_frw_key
                io_read       TYPE REF TO /bobf/if_frw_read
                io_modify     TYPE REF TO /bobf/if_frw_modify
      EXPORTING eo_message    TYPE REF TO /bobf/if_frw_message
                et_failed_key TYPE /bobf/t_frw_key
                et_return     TYPE bapiret2_t.

    "! Atualiza informações de lacre da ordem de frete
    METHODS execute_update_fo_seal_info
      IMPORTING is_ctx        TYPE /bobf/s_frw_ctx_det
                it_key        TYPE /bobf/t_frw_key
                io_read       TYPE REF TO /bobf/if_frw_read
                io_modify     TYPE REF TO /bobf/if_frw_modify
      EXPORTING eo_message    TYPE REF TO /bobf/if_frw_message
                et_failed_key TYPE /bobf/t_frw_key
                et_return     TYPE bapiret2_t.

    "! Verifica e valida se número do lacre já foi utilizado
    METHODS check_seal_number
      IMPORTING it_seal       TYPE /scmtms/t_tor_seal_k
      EXPORTING et_return     TYPE bapiret2_t
      CHANGING  co_message    TYPE REF TO /bobf/if_frw_message
                ct_failed_key TYPE /bobf/t_frw_key.

    "! Prepara mensagens de erro
    METHODS build_message
      IMPORTING it_return     TYPE bapiret2_t
                iv_node       TYPE /bobf/obm_node_key OPTIONAL
                iv_key        TYPE /bobf/conf_key OPTIONAL
                iv_attribute  TYPE string OPTIONAL
      CHANGING  co_message    TYPE REF TO /bobf/if_frw_message
                ct_failed_key TYPE /bobf/t_frw_key
                ct_return     TYPE bapiret2_t.

    "! Atualiza propriedade das células do nó Item
    METHODS update_item_tr_property
      IMPORTING is_ctx        TYPE /bobf/s_frw_ctx_det
                it_key        TYPE /bobf/t_frw_key
                io_read       TYPE REF TO /bobf/if_frw_read
                io_modify     TYPE REF TO /bobf/if_frw_modify
      EXPORTING eo_message    TYPE REF TO /bobf/if_frw_message
                et_failed_key TYPE /bobf/t_frw_key
                et_return     TYPE bapiret2_t.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zcltm_cockpit_frete_event.

    DATA: go_resource    TYPE REF TO /scmtms/cl_ui_tcact_tor_crtres,
          go_event_tor   TYPE REF TO cl_fpm_event,
          gs_new_tor     TYPE ty_prepare,
          gs_aloc_manual TYPE ty_prepare.

    METHODS prepare_message
      IMPORTING io_message  TYPE REF TO /bobf/if_frw_message
      EXPORTING et_messages TYPE fpmgb_t_messages
                et_return   TYPE bapiret2_t
                ev_result   TYPE fpm_event_result.

    METHODS get_data
      IMPORTING io_srvmgr       TYPE REF TO /bobf/if_tra_service_manager
                it_key          TYPE /bobf/t_frw_key
      EXPORTING et_root         TYPE /scmtms/t_tor_root_k
                et_item_tr      TYPE /scmtms/t_tor_item_tr_k
                et_seal         TYPE /scmtms/t_tor_seal_k
                et_exec_info_tr TYPE /scmtms/t_tor_exec_tr_k
                et_child_item   TYPE /scmtms/t_tor_item_tr_k
                et_fu_root      TYPE /scmtms/t_tor_root_k
                et_messages     TYPE fpmgb_t_messages
                et_return       TYPE bapiret2_t
                ev_result       TYPE fpm_event_result.

ENDCLASS.



CLASS zcltm_cockpit_frete_event IMPLEMENTATION.


  METHOD get_instance.

* ---------------------------------------------------------------------------
* Recupera ou cria nova instância da classe
* ---------------------------------------------------------------------------
    IF ( go_instance IS INITIAL ).
      go_instance = NEW zcltm_cockpit_frete_event( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD exit_adapt_event.

* ---------------------------------------------------------------------------
* Exit chamada na classe post-method /SCMTMS/CL_UI_VIEWEXIT_PLN
* ---------------------------------------------------------------------------

    RETURN.

  ENDMETHOD.


  METHOD check_selection_new_tor_fob.

    CHECK io_event->mv_event_id = gc_action-new_tor
       OR io_event->mv_event_id = gc_action-tor_transf.

    IF is_selection-resource[] IS INITIAL.
      " Selecionar pelo menos um caminhão.
      et_messages = VALUE #( ( msgid = gc_message-id msgno = 003 severity = if_xo_const_message=>error ) ).
      ev_result   = if_fpm_constants=>gc_event_result-failed.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_selection_saida_merc.

    DATA: lr_range            TYPE RANGE OF ze_param_low,
          lr_empresa_complete TYPE ty_r_bukrs,
          lr_empresa_partial  TYPE ty_r_bukrs.

    FREE: ev_result, et_messages.

    CHECK io_event->mv_event_id = gc_action-saida_mercadoria.

* ---------------------------------------------------------------------------
* Valida se Ordem de Frete foi preenchido
* ---------------------------------------------------------------------------
    IF is_selection-freight_doc-root[] IS INITIAL.

      " Selecionar pelo menos uma ordem de frete.
      et_messages = VALUE #( BASE et_messages ( msgid = gc_message-id msgno = 022 severity = if_xo_const_message=>error ) ).
      ev_result   = if_fpm_constants=>gc_event_result-failed.
    ENDIF.

    CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Recupera dados da Ordem de Frete
* ---------------------------------------------------------------------------
    DATA(lt_key) = VALUE /bobf/t_frw_key( FOR ls_root_ IN is_selection-freight_doc-root ( key = ls_root_-key ) ).

    me->get_data( EXPORTING io_srvmgr       = me->gs_new_tor-tor_srvmgr
                            it_key          = lt_key
                  IMPORTING et_root         = DATA(lt_root)
                            et_item_tr      = DATA(lt_item_tr)
                            et_seal         = DATA(lt_seal)
                            et_child_item   = DATA(lt_child_item)
                            et_fu_root      = DATA(lt_fu_root)
                            et_messages     = et_messages
                            ev_result       = ev_result ).

    CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Verifica se Ordem de Frete foi salva
* ---------------------------------------------------------------------------
    IF is_selection-freight_doc-root[] IS NOT INITIAL.

      SELECT db_key, tor_id
          FROM /scmtms/d_torrot
          FOR ALL ENTRIES IN @is_selection-freight_doc-root
          WHERE db_key EQ @is_selection-freight_doc-root-key
          INTO TABLE @DATA(lt_root_ok).

      IF sy-subrc EQ 0.
        SORT lt_root_ok BY db_key.
      ENDIF.
    ENDIF.

    LOOP AT is_selection-freight_doc-root[] REFERENCE INTO DATA(ls_root_key).

      DATA(ls_root) = VALUE #( lt_root[ key = ls_root_key->key ] OPTIONAL ).

      CHECK NOT line_exists( lt_root_ok[ db_key = ls_root_key->key ] ).

      " Necessário salvar a OF &1 para seguir com a saída de mercadoria.
      et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                msgno       = 023
                                                severity    = if_xo_const_message=>error
                                                parameter_1 = |{ ls_root-tor_id ALPHA = OUT }| ) ).

      ev_result   = if_fpm_constants=>gc_event_result-failed.
    ENDLOOP.

    CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Recupera parâmetro - Empresas
* ---------------------------------------------------------------------------
    TRY.
        DATA(lo_parameter) = zclca_tabela_parametros=>get_instance( ).

        lo_parameter->m_get_range( EXPORTING iv_modulo = gc_param_saida_empresa-modulo
                                             iv_chave1 = gc_param_saida_empresa-chave1
                                             iv_chave2 = gc_param_saida_empresa-chave2
                                             iv_chave3 = gc_param_saida_empresa-chave3
                                   IMPORTING et_range  = lr_range ).

      CATCH zcxca_tabela_parametros.
        FREE lr_range.
    ENDTRY.

    lr_empresa_complete = VALUE #( FOR ls_range_ IN lr_range WHERE ( low = gc_param_saida_empresa-validacao_completa )
                                                                   ( sign   = gc_range-include
                                                                     option = gc_range-equal
                                                                     low    = ls_range_-high ) ).

    lr_empresa_partial  = VALUE #( FOR ls_range_ IN lr_range WHERE ( low = gc_param_saida_empresa-validacao_parcial )
                                                                   ( sign   = gc_range-include
                                                                     option = gc_range-equal
                                                                     low    = ls_range_-high ) ).

* ---------------------------------------------------------------------------
* Validação para empresas SIM
* ---------------------------------------------------------------------------
    me->check_selection_sm_complete( EXPORTING ir_bukrs      = lr_empresa_complete
                                               it_root       = lt_root
                                               it_item_tr    = lt_item_tr
                                               it_seal       = lt_seal
                                               it_child_item = lt_child_item
                                               it_fu_root    = lt_fu_root
                                     IMPORTING ev_result     = DATA(lv_result)
                                               et_messages   = DATA(lt_messages) ).

    INSERT LINES OF lt_messages INTO TABLE et_messages.
    ev_result = COND #( WHEN lv_result IS NOT INITIAL THEN lv_result ELSE ev_result ).

* ---------------------------------------------------------------------------
* Validação para empresa DISTRIBUIDORA CHARRUA
* ---------------------------------------------------------------------------
    me->check_selection_sm_partial( EXPORTING ir_bukrs      = lr_empresa_partial
                                              it_root       = lt_root
                                              it_item_tr    = lt_item_tr
                                              it_seal       = lt_seal
                                              it_child_item = lt_child_item
                                              it_fu_root    = lt_fu_root
                                    IMPORTING ev_result     = lv_result
                                              et_messages   = lt_messages ).

    INSERT LINES OF lt_messages INTO TABLE et_messages.
    ev_result = COND #( WHEN lv_result IS NOT INITIAL THEN lv_result ELSE ev_result ).

  ENDMETHOD.


  METHOD prepare_new_tor_fob.

    CHECK io_event->mv_event_id = gc_action-new_tor
       OR io_event->mv_event_id = gc_action-tor_transf.

    gs_new_tor = VALUE #( controller          = io_controller
                          log                 = io_log
                          mp_tc               = io_mp_tc
                          tbo_srvmgr          = io_tbo_srvmgr
                          tor_srvmgr          = io_tor_srvmgr
                          block_mw_processing = iv_block_mw_processing ).

  ENDMETHOD.


  METHOD prepare_saida_merc.

    CHECK io_event->mv_event_id = gc_action-saida_mercadoria.

    gs_new_tor = VALUE #( controller          = io_controller
                          log                 = io_log
                          mp_tc               = io_mp_tc
                          tbo_srvmgr          = io_tbo_srvmgr
                          tor_srvmgr          = io_tor_srvmgr
                          block_mw_processing = iv_block_mw_processing ).

  ENDMETHOD.


  METHOD execute_new_tor_fob.

    DATA:
      lt_failed_keys    TYPE /bobf/t_frw_key,
      lt_create_tor     TYPE /scmtms/if_pln_mp=>tt_tor_create_data,
      lr_par_create_tor TYPE REF TO /scmtms/s_pln_a_create_tor,
      lo_message        TYPE REF TO /bobf/if_frw_message,
      lo_change         TYPE REF TO /bobf/if_tra_change,
      lo_exception      TYPE REF TO /bobf/cx_frw,
      lv_tor_type       TYPE /scmtms/tor_type,
      lr_range_param    TYPE RANGE OF ze_param_low,
      lv_type           TYPE /scmtms/tor_type.

    CHECK io_event->mv_event_id = gc_action-new_tor
       OR io_event->mv_event_id = gc_action-tor_transf.

* ---------------------------------------------------------------------------
* Recupera parâmetro
* ---------------------------------------------------------------------------
    TRY.
        DATA(lo_parameter) = zclca_tabela_parametros=>get_instance( ).

        lo_parameter->m_get_range( EXPORTING iv_modulo = gc_param_criar_tipo_doc-modulo
                                             iv_chave1 = gc_param_criar_tipo_doc-chave1
                                             iv_chave2 = gc_param_criar_tipo_doc-chave2
                                             iv_chave3 = gc_param_criar_tipo_doc-chave3
                                   IMPORTING et_range  = lr_range_param ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    IF lr_range_param[] IS INITIAL.
      " Favor cadastrar tipo de documento para operação.
      et_messages = VALUE #( ( msgid    = gc_message-id
                               msgno    = 002
                               severity = if_xo_const_message=>error ) ).
      ev_result   = if_fpm_constants=>gc_event_result-failed.
      RETURN.

    ELSE.

      SORT lr_range_param BY low.

      IF io_event->mv_event_id = gc_action-new_tor.
        READ TABLE lr_range_param ASSIGNING FIELD-SYMBOL(<fs_range_param>)
                                                WITH KEY low = gc_param_criar_tipo_doc-vendas
                                                BINARY SEARCH.

      ELSEIF io_event->mv_event_id = gc_action-tor_transf.
        READ TABLE lr_range_param ASSIGNING <fs_range_param>
                                   WITH KEY low = gc_param_criar_tipo_doc-transf
                                   BINARY SEARCH.
      ENDIF.

      IF sy-subrc IS INITIAL
      OR <fs_range_param>-high IS INITIAL.
        lv_type = <fs_range_param>-high.
      ELSE.
        " Favor cadastrar tipo de documento para operação.
        et_messages = VALUE #( ( msgid    = gc_message-id
                                 msgno    = 002
                                 severity = if_xo_const_message=>error ) ).
        ev_result   = if_fpm_constants=>gc_event_result-failed.
        RETURN.
      ENDIF.

    ENDIF.

* ---------------------------------------------------------------------------
* Cria ordem de frete - lógica da classe /SCMTMS/CL_UI_DIALOG_PLN_TORA
* ---------------------------------------------------------------------------
    TRY.
        " use manual planning operation instead of TBO-action
        DATA(lo_mp) = /scmtms/cl_pln_mp_factory=>get_mp_object( ).

        lt_create_tor = VALUE #( FOR ls_resource_ IN is_selection-resource
                               ( data      = VALUE #( tor_type = lv_type
                                                      vres_key = ls_resource_-key )
                                 tor_count = 1 ) ).

        CALL METHOD lo_mp->tor_create
          EXPORTING
            is_options  = VALUE #( vss_opt-prevent_vss = abap_true )
            it_tor_data = lt_create_tor
          IMPORTING
            eo_change   = lo_change
          CHANGING
            co_message  = lo_message.

        CALL METHOD /scmtms/cl_pln_tbo_factory=>update_tbo_from_changenot
          EXPORTING
            io_change        = lo_change
          IMPORTING
            eo_change_tr_tbo = DATA(lo_change_pln).

        " attention: add TBO - changes ... otherwise data will not be displayed in TC
        IF lo_change IS BOUND AND lo_change_pln IS BOUND.
          lo_change->merge( EXPORTING io_change = lo_change_pln ).
        ENDIF.

        " Dispatch changes and messages
        IF lo_change  IS BOUND OR lo_message IS BOUND.
          gs_new_tor-controller->/bofu/if_fbi_controller~post_syncup_data(
            iv_bo_key  = /scmtms/if_pln_c=>sc_bo_key
            io_change  = lo_change
            io_message = lo_message ).
        ENDIF.

      CATCH /bobf/cx_frw INTO lo_exception.
        " Show error page for exceptional conditions
        gs_new_tor-controller->/bofu/if_fbi_controller~show_error_page( lo_exception ).
    ENDTRY.

    " Set the FPM result indicator
    IF NOT lt_failed_keys IS INITIAL.
      ev_result = if_fpm_constants=>gc_event_result-failed.
    ELSE.
      ev_result = if_fpm_constants=>gc_event_result-ok.
    ENDIF.

  ENDMETHOD.


  METHOD execute_saida_merc.

    DATA: lt_exec_info_tr_s    TYPE STANDARD TABLE OF /scmtms/s_tor_exec_tr_k,
          lo_exec_info_tr_data TYPE REF TO /scmtms/s_tor_exec_tr_k,
          lo_set_load_plan     TYPE REF TO /scmtms/s_tor_a_set_load_plan.

    CHECK io_event->mv_event_id = gc_action-saida_mercadoria.

* ---------------------------------------------------------------------------
* Recupera dados da Ordem de Frete
* ---------------------------------------------------------------------------
    DATA(lt_key) = VALUE /bobf/t_frw_key( FOR ls_root_ IN is_selection-freight_doc-root ( key = ls_root_-key ) ).

    me->get_data( EXPORTING io_srvmgr       = me->gs_new_tor-tor_srvmgr
                            it_key          = lt_key
                  IMPORTING et_root         = DATA(lt_root)
                            et_item_tr      = DATA(lt_item_tr)
                            et_seal         = DATA(lt_seal)
                            et_exec_info_tr = DATA(lt_exec_info_tr)
                            et_messages     = et_messages
                            ev_result       = ev_result ).

    CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

    LOOP AT is_selection-freight_doc-root REFERENCE INTO DATA(ls_root_key).

      DATA(ls_root) = VALUE #( lt_root[ key = ls_root_key->key ] OPTIONAL ).

* ---------------------------------------------------------------------------
* Recupera o próximo ID de numeração
* ---------------------------------------------------------------------------
      lt_exec_info_tr_s   = VALUE #( FOR ls_exec_info_tr_ IN lt_exec_info_tr WHERE ( key = ls_root_key->key ) (  ls_exec_info_tr_ ) ).
      SORT lt_exec_info_tr_s BY execution_id DESCENDING.
      DATA(lv_event_code)   = VALUE #( lt_exec_info_tr_s[ 1 ]-event_code OPTIONAL ).

      IF lv_event_code EQ gc_saida_merc_event-event_code.

        " OF &1 já possui saída de mercadoria.
        et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                  msgno       = 034
                                                  severity    = if_xo_const_message=>error
                                                  parameter_1 = |{ ls_root-tor_id ALPHA = OUT }| ) ).
        ev_result = if_fpm_constants=>gc_event_result-failed.
        CONTINUE.

      ENDIF.

      DATA(lv_execution_id) = CONV /scmtms/item_id( VALUE i( lt_exec_info_tr_s[ 1 ]-execution_id OPTIONAL ) + 10  ).
      lv_execution_id       = |{ lv_execution_id ALPHA = IN }|.

      DATA(lv_execution_key) = /bobf/cl_frw_factory=>get_new_key( ).

* ---------------------------------------------------------------------------
* Prepara novo evento de Saída de Mercadoria
* ---------------------------------------------------------------------------
      CREATE DATA lo_exec_info_tr_data.
      lo_exec_info_tr_data->execution_id    = lv_execution_id.
*      lo_exec_info_tr_data->torstopuuid     = ls_tor_stop_data-key.
*      lo_exec_info_tr_data->ext_loc_uuid    = ls_tor_stop_data-log_loc_uuid.
*      lo_exec_info_tr_data->ext_loc_id      = ls_tor_stop_data-log_locid.
      lo_exec_info_tr_data->actual_tzone    = gc_saida_merc_event-actual_tzone.
      lo_exec_info_tr_data->event_code      = gc_saida_merc_event-event_code.
      lo_exec_info_tr_data->event_status    = gc_saida_merc_event-event_status.
      lo_exec_info_tr_data->trans_activity  = gc_saida_merc_event-trans_activity.
      lo_exec_info_tr_data->execinfo_source = gc_saida_merc_event-execinfo_source.

* ---------------------------------------------------------------------------
* Prepara modificação da OF
* ---------------------------------------------------------------------------
      DATA(lt_mod) = VALUE /bobf/t_frw_modification( ( node           = /scmtms/if_tor_c=>sc_node-executioninformation
                                                       change_mode    = /bobf/if_frw_c=>sc_modify_create
                                                       key            = lv_execution_key
                                                       data           = lo_exec_info_tr_data
                                                       association    = /scmtms/if_tor_c=>sc_association-root-executioninformation_tr
                                                       source_node    = /scmtms/if_tor_c=>sc_node-root
                                                       source_key     = ls_root_key->key
                                                       ) ).

      TRY.
          me->gs_new_tor-tor_srvmgr->modify( EXPORTING it_modification = lt_mod
                                             IMPORTING eo_change       = DATA(lo_change)
                                                       eo_message      = DATA(lo_message) ).
        CATCH /bobf/cx_frw_contrct_violation.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = DATA(lt_messages)
                                     ev_result   = DATA(lv_result)  ).

      INSERT LINES OF lt_messages INTO TABLE et_messages.
      ev_result = COND #( WHEN lv_result IS NOT INITIAL THEN lv_result ELSE ev_result ).

      CHECK lv_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Chama nova ação
* ---------------------------------------------------------------------------
      TRY.
          me->gs_new_tor-tor_srvmgr->do_action( EXPORTING iv_act_key              = /scmtms/if_tor_c=>sc_action-executioninformation_tr-report_event
                                                          it_key                  = VALUE #( ( key = lv_execution_key ) )
                                                IMPORTING eo_change               = lo_change
                                                          eo_message              = lo_message
                                                          et_failed_key           = DATA(lt_failed_key) ).
        CATCH /bobf/cx_frw_contrct_violation.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = lt_messages
                                     ev_result   = lv_result ).

      INSERT LINES OF lt_messages INTO TABLE et_messages.
      ev_result = COND #( WHEN lv_result IS NOT INITIAL THEN lv_result ELSE ev_result ).

      CHECK lv_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Tempo de espera para efetivar as novas ações
* ---------------------------------------------------------------------------
      WAIT UP TO '0.1' SECONDS.

* ---------------------------------------------------------------------------
* Chama ação de "Definir plano de carregamento em Finalizado"
* ---------------------------------------------------------------------------
      CREATE DATA lo_set_load_plan.
      lo_set_load_plan->load_plan_item          = gc_load_plan_sts-complete.
      lo_set_load_plan->set_load_plan_item      = abap_true.
      lo_set_load_plan->disable_event_report    = abap_true.
      lo_set_load_plan->ui_action_source        = abap_true.
      lo_set_load_plan->no_check                = abap_true.
      lo_set_load_plan->only_top_level_item     = abap_true.

      TRY.
          me->gs_new_tor-tor_srvmgr->do_action( EXPORTING iv_act_key              = /scmtms/if_tor_c=>sc_action-root-set_load_plan_status
                                                          it_key                  = VALUE #( ( key = ls_root_key->key ) )
                                                          is_parameters           = lo_set_load_plan
                                                IMPORTING eo_change               = lo_change
                                                          eo_message              = lo_message
                                                          et_failed_key           = lt_failed_key ).
        CATCH /bobf/cx_frw_contrct_violation.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = lt_messages
                                     ev_result   = lv_result ).

      INSERT LINES OF lt_messages INTO TABLE et_messages.
      ev_result = COND #( WHEN lv_result IS NOT INITIAL THEN lv_result ELSE ev_result ).

      CHECK lv_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Atualiza página
* ---------------------------------------------------------------------------
      CALL METHOD /scmtms/cl_pln_tbo_factory=>update_tbo_from_changenot
        EXPORTING
          io_change        = lo_change
        IMPORTING
          eo_change_tr_tbo = DATA(lo_change_pln).

      " attention: add TBO - changes ... otherwise data will not be displayed in TC
      IF lo_change IS BOUND AND lo_change_pln IS BOUND.
        lo_change->merge( EXPORTING io_change = lo_change_pln ).
      ENDIF.

      " Dispatch changes and messages
      IF lo_change  IS BOUND OR lo_message IS BOUND.
        gs_new_tor-controller->/bofu/if_fbi_controller~post_syncup_data(
          iv_bo_key  = /scmtms/if_pln_c=>sc_bo_key
          io_change  = lo_change
          io_message = lo_message ).
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD prepare_old_tor.

    CHECK io_event->mv_event_id = gc_action-new_tor
       OR io_event->mv_event_id = gc_action-tor_transf.

* ---------------------------------------------------------------------------
* Cria evento Standard
* ---------------------------------------------------------------------------
    io_event->create_by_id( EXPORTING iv_event_id    = gc_action-old_tor
                                      io_event_data  = io_event->mo_event_data
                                      iv_action_type = if_fpm_constants=>gc_action_type-standard
                            RECEIVING ro_event       = go_event_tor ).

* ---------------------------------------------------------------------------
* Prepara a informação
* ---------------------------------------------------------------------------
    go_resource = NEW /scmtms/cl_ui_tcact_tor_crtres( ).

    go_resource->/scmtms/if_ui_tcact~prepare( EXPORTING io_controller          = io_controller
                                                        io_log                 = io_log
                                                        io_mp_tc               = io_mp_tc
                                                        io_tbo_srvmgr          = io_tbo_srvmgr
                                                        io_tor_srvmgr          = io_tor_srvmgr
                                                        io_event               = go_event_tor
                                                        iv_block_mw_processing = iv_block_mw_processing
                                              IMPORTING ev_result              = ev_result
                                                        et_messages            = et_messages ).

* ---------------------------------------------------------------------------
* Recupera linhas selecionadas
* ---------------------------------------------------------------------------
    go_resource->/scmtms/if_ui_tcact~get_selection( io_event           = go_event_tor
                                                    iv_clear_selection = abap_false ).



  ENDMETHOD.


  METHOD execute_old_tor.

    CHECK io_event->mv_event_id = gc_action-new_tor
       OR io_event->mv_event_id = gc_action-tor_transf.

* ---------------------------------------------------------------------------
* Executa operação standard
* ---------------------------------------------------------------------------
    go_resource->/scmtms/if_ui_tcact~execute( EXPORTING io_event                   = go_event_tor
                                              IMPORTING ev_suppress_dirty_handling = ev_suppress_dirty_handling
                                                        ev_trigger_save            = ev_trigger_save
                                                        ev_trigger_refresh         = ev_trigger_refresh
                                                        ev_trigger_deselect        = ev_trigger_deselect
                                                        ev_result                  = ev_result
                                                        et_messages                = et_messages ).

** ---------------------------------------------------------------------------
** Cria evento Standard
** ---------------------------------------------------------------------------
*    io_event->create_by_id( EXPORTING iv_event_id    = gc_action-close_dialog
*                                      iv_action_type = if_fpm_constants=>gc_action_type-standard
*                           RECEIVING  ro_event       = DATA(lo_event_dialog) ).
*
*    lo_event_dialog->mo_event_data->set_value( iv_key   = 'IN_DIALOG_MODE'
*                                               iv_value = abap_true ).
*
*    lo_event_dialog->mo_event_data->set_value( iv_key   = 'DIALOG_BOX_ID'
*                                               iv_value = 'POPUP' ).
*
*    lo_event_dialog->mo_event_data->set_value( iv_key   = 'DIALOG_BUTTON_ACTION'
*                                               iv_value = 'OK' ).
*
* ---------------------------------------------------------------------------
* Chama lógica standard para criação da OF
* ---------------------------------------------------------------------------
*    DATA: lo_assist  TYPE REF TO Cl_Fpm_Form_Uibb_Assist.
*
*    lo_assist = NEW Cl_Fpm_Form_Uibb_Assist( ).
*
*    " Initialize feeder
*    lo_assist->init_feeder( ).
*
*    lo_assist->dispatch_process_event(
*     EXPORTING
*       io_event = io_event
*     IMPORTING
*       ev_result = ev_result
*    ).

  ENDMETHOD.


  METHOD execute_update_fo_equi_info.

    DATA: lo_tor_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
          lt_item_tr      TYPE /scmtms/t_tor_item_tr_k,
          lt_item_tr_u    TYPE /scmtms/t_tor_item_tr_k,
          lt_child_item   TYPE /scmtms/t_tor_item_tr_k,
          lt_stage_utlz   TYPE /scmtms/t_tor_stop_succ_util_k,
          lt_mod          TYPE /bobf/t_frw_modification,
          lt_root         TYPE /scmtms/t_tor_root_k,
          lt_fu_root      TYPE /scmtms/t_tor_root_k,
          lt_key          TYPE /bobf/t_frw_key,
          lt_fu_key_found TYPE SORTED TABLE OF /scmtms/s_tor_root_k-key
                          WITH UNIQUE KEY table_line.

    FREE: eo_message,
          et_failed_key,
          et_return.

*    lo_tor_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( is_ctx-bo_key ).

* ---------------------------------------------------------------------------
* Recupera os dados e verificamos quais são Ordens de Frete
* ---------------------------------------------------------------------------
    TRY.
        IF it_key IS NOT INITIAL.
          io_read->retrieve( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-root
                                       it_key         = it_key
                                       iv_fill_data   = abap_true
                             IMPORTING eo_message     = DATA(lo_message)
                                       et_data        = lt_root
                                       et_failed_key  = DATA(lt_failed) ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Filtra somente as ordens de frete
* ---------------------------------------------------------------------------
    LOOP AT it_key REFERENCE INTO DATA(ls_key).

      READ TABLE lt_root REFERENCE INTO DATA(ls_root) WITH KEY key = ls_key->key.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF ls_root->tor_cat NE /scmtms/if_tor_const=>sc_tor_category-active.
        CONTINUE.
      ENDIF.

      INSERT ls_key->* INTO TABLE lt_key[].

    ENDLOOP.

    IF lt_key IS INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera Equipamentos da FO
* ---------------------------------------------------------------------------
    TRY.
        IF it_key IS NOT INITIAL.
          io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-root
                                                      it_key         = lt_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr
                                                      iv_fill_data   = abap_true
                                            IMPORTING eo_message     = lo_message
                                                      et_data        = lt_item_tr
                                                      et_target_key  = DATA(lt_item_tr_root_key)
                                                      et_failed_key  = lt_failed ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera associação do Equipamento da FO com a FU
* ---------------------------------------------------------------------------
    TRY.
        IF lt_item_tr_root_key IS NOT INITIAL.
          io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-item_tr
                                                      it_key         = lt_item_tr_root_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-item_tr-child_item
                                                      iv_fill_data   = abap_true
                                            IMPORTING eo_message     = lo_message
                                                      et_data        = lt_child_item
                                                      et_failed_key  = lt_failed ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera etapas do compartimento da FO (capacidade utilizada)
* ---------------------------------------------------------------------------
    TRY.
        IF lt_item_tr_root_key IS NOT INITIAL.
          io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-item_tr
                                                      it_key         = lt_item_tr_root_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-item_tr-stage_utlz_highest
                                                      iv_fill_data   = abap_true
                                            IMPORTING eo_message     = lo_message
                                                      et_data        = lt_stage_utlz
                                                      et_failed_key  = lt_failed ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera dados da FU
* ---------------------------------------------------------------------------
    DATA(lt_fu_key) = VALUE /bobf/t_frw_key( FOR ls_child_item_ IN lt_child_item WHERE ( fu_root_key IS NOT INITIAL AND fu_root_key NE gc_constants-fu_root_key_0 )
                                           ( key = ls_child_item_-fu_root_key ) ). "#EC CI_SORTSEQ

    TRY.
        IF lt_fu_key IS NOT INITIAL.
          io_read->retrieve( EXPORTING iv_node       = /scmtms/if_tor_c=>sc_node-root
                                       it_key        = lt_fu_key
                             IMPORTING eo_message    = lo_message
                                       et_data       = lt_fu_root
                                       et_failed_key = lt_failed ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera relação Compartimento X Lacre
* ---------------------------------------------------------------------------
    zcltm_seal_number=>get_qty_seals( EXPORTING it_key        = lt_item_tr_root_key    " Key Table
                                                io_read       = io_read                " Interface to Read Data
                                      RECEIVING rt_qtde_lacre = DATA(lt_qtd_lacre) ).  " Quantidade de Lacres por Compartimento

* ---------------------------------------------------------------------------
* Níves: Conjunto -> Cavalo -> Carreta
*
* Atualizamos a informação a partir do último nível
* ---------------------------------------------------------------------------

* ---------------------------------------------------------------------------
* Atualiza informação de veículo - Carreta / Compartimento
* ---------------------------------------------------------------------------
    LOOP AT lt_item_tr REFERENCE INTO DATA(ls_item_tr).

      FREE: lt_fu_key_found.

      CHECK ls_item_tr->item_cat = /scmtms/if_tor_const=>sc_tor_item_category-ct_item.

      " Busca pela associação
      LOOP AT lt_child_item REFERENCE INTO DATA(ls_child_item) USING KEY item_parent_key WHERE item_parent_key = ls_item_tr->key. "#EC CI_NESTED
        INSERT ls_child_item->fu_root_key INTO TABLE lt_fu_key_found.
      ENDLOOP.

      " Busca o nível anterior
      READ TABLE lt_item_tr INTO DATA(ls_parent_item) WITH KEY key = ls_item_tr->item_parent_key.

      IF sy-subrc NE 0.
        CLEAR ls_parent_item.
      ENDIF.

      TRY.
          " Recupera dados da FU
          DATA(lv_fu_found) = lines( lt_fu_key_found[] ).
          DATA(lv_fu_key)   = lt_fu_key_found[ 1 ].
          DATA(ls_fu_root)  = lt_fu_root[ key = lv_fu_key ].
        CATCH cx_root.
          CLEAR ls_fu_root.
      ENDTRY.

      TRY.
          " Recupera dados da etapa (utilização)
          DATA(ls_stage) = lt_stage_utlz[ item_key = ls_item_tr->key ]. "#EC CI_SORTSEQ
        CATCH cx_root.
          CLEAR ls_stage.
      ENDTRY.


      " Atualiza dados do equipamento
      ls_item_tr->zz_fu_db_key   = COND #( WHEN lv_fu_found = 1
                                           THEN lv_fu_key
                                           ELSE VALUE /bobf/conf_key( ) ).

      ls_item_tr->zz_fu_tor_id   = COND #( WHEN lv_fu_found = 1
                                           THEN ls_fu_root-tor_id
                                           WHEN lv_fu_found > 1
                                           THEN |{ lv_fu_found } FUs!|
                                           ELSE space ).

      " Atualiza percentual de utilização e aplica arredondamento similar ao standard.
      ls_item_tr->zz_fu_max_util = COND #( WHEN lv_fu_found = 1 AND ls_stage-util_max GT 1
                                           THEN ls_stage-util_max
                                           WHEN lv_fu_found = 1 AND ls_stage-util_max GT 0
                                           THEN 1
                                           ELSE 0 ).

      " Recupera quantidade de lacre por compartimento
      READ TABLE lt_qtd_lacre INTO DATA(ls_qtd_lacre) WITH KEY res_id   = ls_parent_item-res_id
                                                               sequence = ls_item_tr->ct_seq
                                                               BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_qtd_lacre.
      ENDIF.

      ls_item_tr->zz_seal_number_total = ls_qtd_lacre-zz_seal_number_total.

      INSERT ls_item_tr->* INTO TABLE lt_item_tr_u.

    ENDLOOP.

* ---------------------------------------------------------------------------
* Atualiza informação do veículo - Cavalo
* ---------------------------------------------------------------------------
    LOOP AT lt_item_tr REFERENCE INTO ls_item_tr.

      CHECK ls_item_tr->item_cat = /scmtms/if_tor_const=>sc_tor_item_category-pv_item.

      TRY.
          " Recupera dados da etapa (utilização)
          ls_stage = lt_stage_utlz[ item_key = ls_item_tr->key ]. "#EC CI_SORTSEQ
        CATCH cx_root.
          CLEAR ls_stage.
      ENDTRY.

      " Atualiza dados do equipamento
      ls_item_tr->zz_fu_db_key   = VALUE /bobf/conf_key( ).
      ls_item_tr->zz_fu_tor_id   = space.

      " Atualiza percentual de utilização e aplica arredondamento similar ao standard.
      ls_item_tr->zz_fu_max_util = COND #( WHEN ls_stage-util_max GT 1
                                           THEN ls_stage-util_max
                                           WHEN ls_stage-util_max GT 0
                                           THEN 1
                                           ELSE 0 ).

      ls_item_tr->zz_seal_number_total = 0.

      LOOP AT lt_item_tr REFERENCE INTO DATA(ls_item_tr_child) USING KEY item_parent_key WHERE item_parent_key = ls_item_tr->key. "#EC CI_NESTED
        ADD ls_item_tr_child->zz_seal_number_total TO ls_item_tr->zz_seal_number_total.
      ENDLOOP.

      INSERT ls_item_tr->* INTO TABLE lt_item_tr_u.

    ENDLOOP.

* ---------------------------------------------------------------------------
* Atualiza informação do veículo - Conjunto
* ---------------------------------------------------------------------------
    LOOP AT lt_item_tr REFERENCE INTO ls_item_tr.

      CHECK ls_item_tr->item_cat = /scmtms/if_tor_const=>sc_tor_item_category-av_item.

      TRY.
          " Recupera dados da etapa (utilização)
          ls_stage = lt_stage_utlz[ item_key = ls_item_tr->key ]. "#EC CI_SORTSEQ
        CATCH cx_root.
          CLEAR ls_stage.
      ENDTRY.

      " Atualiza dados do equipamento
      ls_item_tr->zz_fu_db_key   = VALUE /bobf/conf_key( ).
      ls_item_tr->zz_fu_tor_id   = space.

      " Atualiza percentual de utilização e aplica arredondamento similar ao standard.
      ls_item_tr->zz_fu_max_util = COND #( WHEN ls_stage-util_max GT 1
                                           THEN ls_stage-util_max
                                           WHEN ls_stage-util_max GT 0
                                           THEN 1
                                           ELSE 0 ).

      ls_item_tr->zz_seal_number_total = 0.

      LOOP AT lt_item_tr REFERENCE INTO ls_item_tr_child USING KEY item_parent_key WHERE item_parent_key = ls_item_tr->key. "#EC CI_NESTED
        ADD ls_item_tr_child->zz_seal_number_total TO ls_item_tr->zz_seal_number_total.
      ENDLOOP.

      INSERT ls_item_tr->* INTO TABLE lt_item_tr_u.

    ENDLOOP.

* ---------------------------------------------------------------------------
* Atualiza os equipamentos com os dados da FU
* ---------------------------------------------------------------------------
    CHECK lt_item_tr_u IS NOT INITIAL.

    " Informa apenas os campos que queremos atualizar
    DATA(lt_changed_fields) = VALUE /bobf/t_frw_name( ( gc_equi_fields-zz_fu_db_key )
                                                      ( gc_equi_fields-zz_fu_tor_id )
                                                      ( gc_equi_fields-zz_fu_max_util )
                                                      ( gc_equi_fields-zz_seal_number_total ) ).

    " Atualiza equipamentos
    /scmtms/cl_mod_helper=>mod_update_multi( EXPORTING iv_node           = /scmtms/if_tor_c=>sc_node-item_tr
                                                       it_data           = lt_item_tr_u
                                                       it_changed_fields = lt_changed_fields
                                                       iv_bo_key         = /scmtms/if_tor_c=>sc_bo_key
                                              CHANGING ct_mod            = lt_mod ).

    io_modify->do_modify( it_modification = lt_mod ).

    IF lo_message IS NOT INITIAL.
      /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                             CHANGING  ct_bapiret2 = et_return ).
    ENDIF.

  ENDMETHOD.


  METHOD execute_update_fo_seal_info.

    DATA: lo_tor_srv_mgr TYPE REF TO /bobf/if_tra_service_manager,
          lt_item_tr     TYPE /scmtms/t_tor_item_tr_k,
          lt_seal        TYPE /scmtms/t_tor_seal_k,
          lt_seal_u      TYPE /scmtms/t_tor_seal_k,
          lt_mod         TYPE /bobf/t_frw_modification,
          lt_fu_root     TYPE /scmtms/t_tor_root_k.

    FREE: eo_message,
          et_failed_key,
          et_return.

*    lo_tor_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( is_ctx-bo_key ).

* ---------------------------------------------------------------------------
* Recupera Equipamentos da FO
* ---------------------------------------------------------------------------
    TRY.
        IF it_key IS NOT INITIAL.
          io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-root
                                                      it_key         = it_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr
                                                      iv_fill_data   = abap_true
                                            IMPORTING eo_message     = DATA(lo_message)
                                                      et_data        = lt_item_tr
                                                      et_failed_key  = DATA(lt_failed) ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera Lacres da FO
* ---------------------------------------------------------------------------
    TRY.
        IF it_key IS NOT INITIAL.
          io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-root
                                                      it_key         = it_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-seal
                                                      iv_fill_data   = abap_true
                                            IMPORTING eo_message     = lo_message
                                                      et_data        = lt_seal
                                                      et_failed_key  = lt_failed ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    CONSTANTS:
      lc_fu_root_key TYPE /scmtms/tor_key VALUE '00000000000000000000000000000000'.
* ---------------------------------------------------------------------------
* Recupera dados da FU
* ---------------------------------------------------------------------------
    DATA(lt_fu_key) = VALUE /bobf/t_frw_key( FOR ls_item_tr_ IN lt_item_tr WHERE ( fu_root_key IS NOT INITIAL AND fu_root_key NE lc_fu_root_key )
                                           ( key = ls_item_tr_-zz_fu_db_key ) ). "#EC CI_SORTSEQ

    TRY.
        IF lt_fu_key IS NOT INITIAL.
          io_read->retrieve( EXPORTING iv_node       = /scmtms/if_tor_c=>sc_node-root
                                       it_key        = lt_fu_key
                             IMPORTING eo_message    = lo_message
                                       et_data       = lt_fu_root
                                       et_failed_key = lt_failed ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = et_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    eo_message = lo_message.

* ---------------------------------------------------------------------------
* Valida se lacre já foi utilizado
* ---------------------------------------------------------------------------
    me->check_seal_number( EXPORTING it_seal       = lt_seal "lt_seal_u
                           IMPORTING et_return     = et_return
                           CHANGING  co_message    = eo_message
                                     ct_failed_key = et_failed_key ).

    IF et_return IS NOT INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Atualiza os lacres com os dados da FU
* ---------------------------------------------------------------------------
*    CHECK lt_seal_u IS NOT INITIAL.
*
*    " Informa apenas os campos que queremos atualizar
*    DATA(lt_changed_fields) = VALUE /bobf/t_frw_name( ( gc_seal_fields-zz_fu_erp_plant_id ) ).
*
*    " Atualiza equipamentos
*    /scmtms/cl_mod_helper=>mod_update_multi( EXPORTING iv_node           = /scmtms/if_tor_c=>sc_node-seal
*                                                       it_data           = lt_seal_u
*                                                       it_changed_fields = lt_changed_fields
*                                                       iv_bo_key         = /scmtms/if_tor_c=>sc_bo_key
*                                              CHANGING ct_mod            = lt_mod ).
*
*    io_modify->do_modify( it_modification = lt_mod ).
*
*    IF lo_message IS NOT INITIAL.
*      /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
*                                                             CHANGING  ct_bapiret2 = et_return ).
*    ENDIF.

  ENDMETHOD.


  METHOD check_seal_number.

    DATA: lt_return TYPE bapiret2_t.

    FREE: et_return.

    IF co_message IS INITIAL.
      co_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera todos os números de lacre utilizados
* ---------------------------------------------------------------------------
    IF it_seal IS NOT INITIAL.

      SELECT seal_number,
             erp_plant_id,
             db_key,
             root_key,
             tor_id,
             parent_key
          FROM zi_tm_vh_tor_seal_number
          INTO TABLE @DATA(lt_seal_all)
          FOR ALL ENTRIES IN @it_seal
          WHERE seal_number  = @it_seal-seal_number
            AND erp_plant_id = @it_seal-zz_fu_erp_plant_id.

      IF sy-subrc EQ 0.
        SORT lt_seal_all BY seal_number erp_plant_id.
      ENDIF.
    ENDIF.
    CONSTANTS:
      lc_attribute  TYPE string         VALUE 'SEAL_NUMBER',
      lc_error      TYPE bapiret2-type   VALUE 'E',
      lc_id_cockpit TYPE bapiret2-id     VALUE 'ZTM_COCKPIT_FRETE',
      lc_number_11  TYPE bapiret2-number VALUE '011',
      lc_number_10  TYPE bapiret2-number VALUE '010'.
* ---------------------------------------------------------------------------
* Verifica a utilização do número de série
* ---------------------------------------------------------------------------
    LOOP AT it_seal REFERENCE INTO DATA(ls_seal).

      FREE: lt_return.

      READ TABLE lt_seal_all TRANSPORTING NO FIELDS WITH KEY seal_number  = ls_seal->seal_number
                                                             erp_plant_id = ls_seal->zz_fu_erp_plant_id
                                                             BINARY SEARCH.

      CHECK sy-subrc EQ 0.

      LOOP AT lt_seal_all REFERENCE INTO DATA(ls_seal_all) FROM sy-tabix WHERE seal_number  = ls_seal->seal_number
                                                                           AND erp_plant_id = ls_seal->zz_fu_erp_plant_id. "#EC CI_NESTED

        IF  ls_seal->key      NE ls_seal_all->db_key
        AND ls_seal->root_key NE ls_seal_all->root_key.

          " Lacre &1 utilizado na ordem de frete &2.
          lt_return = VALUE #( BASE lt_return ( type = lc_error id = lc_id_cockpit number = lc_number_11 message_v1 = ls_seal->seal_number message_v2 = |{ ls_seal_all->tor_id ALPHA = OUT }| ) ).
          EXIT.

        ENDIF.

        IF  ls_seal->key      NE ls_seal_all->db_key
        AND ls_seal->root_key EQ ls_seal_all->root_key.

          " Lacre &1 duplicado na mesma ordem de frete &2.
          lt_return = VALUE #( BASE lt_return ( type = lc_error id = lc_id_cockpit number = lc_number_10 message_v1 = ls_seal->seal_number message_v2 = |{ ls_seal_all->tor_id ALPHA = OUT }| ) ).
          EXIT.

        ENDIF.

      ENDLOOP.

      me->build_message( EXPORTING it_return     = lt_return
                                   iv_node       = /scmtms/if_tor_c=>sc_node-seal
                                   iv_key        = ls_seal->key
                                   iv_attribute  = lc_attribute
                         CHANGING  co_message    = co_message
                                   ct_failed_key = ct_failed_key
                                   ct_return     = et_return ).

    ENDLOOP.

  ENDMETHOD.


  METHOD build_message.

    CHECK it_return IS NOT INITIAL.

    LOOP AT it_return REFERENCE INTO DATA(ls_return).

      ct_return = VALUE #( BASE ct_return ( ls_return->* ) ).

      co_message->add_message( is_msg       = VALUE #( msgty = ls_return->type
                                                       msgid = ls_return->id
                                                       msgno = ls_return->number
                                                       msgv1 = ls_return->message_v1
                                                       msgv2 = ls_return->message_v2
                                                       msgv3 = ls_return->message_v3
                                                       msgv4 = ls_return->message_v4 )
                               iv_node      = iv_node
                               iv_key       = iv_key
                               iv_attribute = iv_attribute ).

    ENDLOOP.

    IF iv_key IS NOT INITIAL AND NOT line_exists( ct_failed_key[ KEY key_sort COMPONENTS key = iv_key ] ).
      ct_failed_key = VALUE #( BASE ct_failed_key ( key = iv_key ) ).
    ENDIF.

  ENDMETHOD.


  METHOD update_item_tr_property.

    DATA: "lo_tor_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
      lo_set_property TYPE REF TO /bobf/cl_lib_h_set_property,
      lt_item_tr      TYPE /scmtms/t_tor_item_tr_k,
      lt_seal         TYPE /scmtms/t_tor_seal_k,
      lt_seal_u       TYPE /scmtms/t_tor_seal_k,
      lt_mod          TYPE /bobf/t_frw_modification,
      lt_fu_root      TYPE /scmtms/t_tor_root_k,
      lt_return       TYPE bapiret2_t.
    CONSTANTS:
      lc_item_cat      TYPE /scmtms/d_torite-item_cat VALUE 'PRD',
      lc_erp_comp_code TYPE /scmtms/d_torite-erp_comp_code VALUE '2500'.
*    lo_tor_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( is_ctx-bo_key ).

* ---------------------------------------------------------------------------
* Recupera dados dos Equipamentos
* ---------------------------------------------------------------------------
    TRY.
        IF it_key IS NOT INITIAL.
          io_read->retrieve( EXPORTING iv_node       = /scmtms/if_tor_c=>sc_node-item_tr
                                       it_key        = it_key
                                       iv_fill_data  = abap_true
                             IMPORTING eo_message    = DATA(lo_message)
                                       et_data       = lt_item_tr
                                       et_failed_key = DATA(lt_failed) ).

          IF lo_message IS NOT INITIAL.
            /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = lo_message
                                                                   CHANGING  ct_bapiret2 = lt_return ).
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera instância da propriedade
* ---------------------------------------------------------------------------
    CREATE OBJECT lo_set_property
      EXPORTING
        is_context = is_ctx
        io_modify  = io_modify.

* ---------------------------------------------------------------------------
* Atualiza campos da tela (linha de veículo)
* ---------------------------------------------------------------------------
    LOOP AT lt_item_tr REFERENCE INTO DATA(ls_item_tr).

      CHECK ls_item_tr->item_cat = /scmtms/if_tor_const=>sc_tor_item_category-av_item
         OR ls_item_tr->item_cat = /scmtms/if_tor_const=>sc_tor_item_category-pv_item.

      DATA(lv_seal_number_total) = 0.

      " Atualiza os campos de lacre para modo somente leitura
      WHILE lv_seal_number_total < gc_equi_fields-max_seal_number. "#EC CI_NESTED

        ADD 1 TO lv_seal_number_total.

        DATA(lv_attribute_name) = |{ gc_equi_fields-zz_seal_number_ }{ lv_seal_number_total }|.

        CALL METHOD lo_set_property->set_attribute_read_only
          EXPORTING
            iv_attribute_name = lv_attribute_name
            iv_key            = ls_item_tr->key
            iv_value          = abap_true.

      ENDWHILE.

      CALL METHOD lo_set_property->set_attribute_read_only
        EXPORTING
          iv_attribute_name = gc_equi_fields-zz_sample_envelope
          iv_key            = ls_item_tr->key
          iv_value          = abap_true.

    ENDLOOP.
    CONSTANTS:
      lc_zz_fu_tor_id TYPE /scmtms/tor_id VALUE '00000000000000000000'.
* ---------------------------------------------------------------------------
* Atualiza campos da tela (linha de compartimento)
* ---------------------------------------------------------------------------
    LOOP AT lt_item_tr REFERENCE INTO ls_item_tr.

      CHECK ls_item_tr->item_cat = /scmtms/if_tor_const=>sc_tor_item_category-ct_item.

*      " Recupera quantidade de lacre por compartimento
      DATA(lv_qtd_lacre) = ls_item_tr->zz_seal_number_total.
      DATA(lv_sample_envelope) = abap_true.

      " Verifica se unidade de frete está associada
      IF ls_item_tr->zz_fu_tor_id IS INITIAL OR ls_item_tr->zz_fu_tor_id EQ lc_zz_fu_tor_id.
        CLEAR: lv_qtd_lacre, lv_sample_envelope.
      ENDIF.

      " Verifica se a utilização máxima foi preenchida (100%)
      IF ls_item_tr->zz_fu_max_util < 100.
        SELECT COUNT(*) FROM /scmtms/d_torite        "#EC CI_SEL_NESTED
        WHERE parent_key    = @ls_item_tr->zz_fu_db_key
          AND item_cat      = @lc_item_cat
          AND erp_comp_code = @lc_erp_comp_code.
        IF sy-subrc <> 0.
          CLEAR: lv_qtd_lacre, lv_sample_envelope.
        ENDIF.
      ENDIF.

      " Atualiza os campos de lacre para modo somente leitura
      DO gc_equi_fields-max_seal_number TIMES.           "#EC CI_NESTED

        lv_attribute_name = |{ gc_equi_fields-zz_seal_number_ }{ sy-index }|.

        IF sy-index <= lv_qtd_lacre.

          CALL METHOD lo_set_property->set_attribute_read_only
            EXPORTING
              iv_attribute_name = lv_attribute_name
              iv_key            = ls_item_tr->key
              iv_value          = abap_false.

        ELSE.

          CALL METHOD lo_set_property->set_attribute_read_only
            EXPORTING
              iv_attribute_name = lv_attribute_name
              iv_key            = ls_item_tr->key
              iv_value          = abap_true.

        ENDIF.

      ENDDO.

      IF lv_sample_envelope EQ abap_false.

        CALL METHOD lo_set_property->set_attribute_read_only
          EXPORTING
            iv_attribute_name = gc_equi_fields-zz_sample_envelope
            iv_key            = ls_item_tr->key
            iv_value          = abap_true.

      ELSE.

        CALL METHOD lo_set_property->set_attribute_read_only
          EXPORTING
            iv_attribute_name = gc_equi_fields-zz_sample_envelope
            iv_key            = ls_item_tr->key
            iv_value          = abap_false.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_message.

    FREE: et_return, et_messages.

    CHECK io_message IS NOT INITIAL.

    /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = io_message
                                                           CHANGING  ct_bapiret2 = et_return ).

    et_messages = VALUE #( BASE et_messages FOR ls_return_ IN et_return ( msgid       = ls_return_-id
                                                                          msgno       = ls_return_-number
                                                                          severity    = ls_return_-type
                                                                          parameter_1 = ls_return_-message_v1
                                                                          parameter_2 = ls_return_-message_v2
                                                                          parameter_3 = ls_return_-message_v3
                                                                          parameter_4 = ls_return_-message_v4 ) ).

    IF line_exists( et_return[ type = if_xo_const_message=>error ] ).
      ev_result = if_fpm_constants=>gc_event_result-failed.
    ENDIF.

  ENDMETHOD.


  METHOD get_data.

    FREE: et_root,
          et_item_tr,
          et_seal,
          et_exec_info_tr,
          et_child_item,
          et_fu_root,
          et_messages,
          et_return,
          ev_result.

    CHECK it_key IS NOT INITIAL.

* ---------------------------------------------------------------------------
* Recupera dados da Ordem de Frete
* ---------------------------------------------------------------------------
    TRY.
        me->gs_new_tor-tor_srvmgr->retrieve( EXPORTING iv_node_key             = /scmtms/if_tor_c=>sc_node-root
                                                       it_key                  = it_key
                                             IMPORTING eo_message              = DATA(lo_message)
                                                       eo_change               = DATA(lo_change)
                                                       et_data                 = et_root
                                                       et_failed_key           = DATA(lt_failed) ).
      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.

    me->prepare_message( EXPORTING io_message  = lo_message
                         IMPORTING et_messages = et_messages
                                   et_return   = et_return
                                   ev_result   = ev_result ).

    CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

* ---------------------------------------------------------------------------
* Recupera Equipamentos da Ordem de Frete
* ---------------------------------------------------------------------------
    IF et_item_tr IS REQUESTED.

      TRY.
          me->gs_new_tor-tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                                        it_key         = it_key
                                                                        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr
                                                                        iv_fill_data   = abap_true
                                                              IMPORTING eo_message     = lo_message
                                                                        et_data        = et_item_tr
                                                                        et_target_key  = DATA(lt_item_tr_root_key)
                                                                        et_failed_key  = lt_failed ).
        CATCH cx_root.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = et_messages
                                     et_return   = et_return
                                     ev_result   = ev_result  ).

      CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

    ENDIF.

* ---------------------------------------------------------------------------
* Recupera Lacres da Ordem de Frete
* ---------------------------------------------------------------------------
    IF et_seal IS REQUESTED.

      TRY.
          me->gs_new_tor-tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                                        it_key         = it_key
                                                                        iv_association = /scmtms/if_tor_c=>sc_association-root-seal
                                                                        iv_fill_data   = abap_true
                                                              IMPORTING eo_message     = lo_message
                                                                        et_data        = et_seal
                                                                        et_failed_key  = lt_failed ).
        CATCH cx_root.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = et_messages
                                     et_return   = et_return
                                     ev_result   = ev_result  ).

      CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

    ENDIF.

* ---------------------------------------------------------------------------
* Recupera informações de execução da Ordem de Frete
* ---------------------------------------------------------------------------
    IF et_exec_info_tr IS REQUESTED.

      TRY.
          me->gs_new_tor-tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                                        it_key         = it_key
                                                                        iv_association = /scmtms/if_tor_c=>sc_association-root-executioninformation_tr
                                                                        iv_fill_data   = abap_true
                                                              IMPORTING eo_message     = lo_message
                                                                        et_data        = et_exec_info_tr
                                                                        et_failed_key  = lt_failed ).
        CATCH cx_root.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = et_messages
                                     et_return   = et_return
                                     ev_result   = ev_result  ).

      CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

    ENDIF.

* ---------------------------------------------------------------------------
* Recupera associação do Equipamento da Ordem de Frete x Unidade de Frete
* ---------------------------------------------------------------------------
    IF et_child_item IS REQUESTED
    OR et_fu_root    IS REQUESTED.

      TRY.
          IF lt_item_tr_root_key IS NOT INITIAL.
            me->gs_new_tor-tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-item_tr
                                                                          it_key         = lt_item_tr_root_key
                                                                          iv_association = /scmtms/if_tor_c=>sc_association-item_tr-child_item
                                                                          iv_fill_data   = abap_true
                                                                IMPORTING eo_message     = lo_message
                                                                          et_data        = et_child_item
                                                                          et_failed_key  = lt_failed ).
          ENDIF.
        CATCH cx_root.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = et_messages
                                     et_return   = et_return
                                     ev_result   = ev_result  ).

      CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

    ENDIF.

* ---------------------------------------------------------------------------
* Recupera dados da Unidade de Frete
* ---------------------------------------------------------------------------
    IF et_fu_root IS REQUESTED.

      DATA(lt_fu_key) = VALUE /bobf/t_frw_key( FOR ls_child_item_ IN et_child_item WHERE ( fu_root_key IS NOT INITIAL AND fu_root_key NE gc_constants-fu_root_key_0 )
                                             ( key = ls_child_item_-fu_root_key ) ).

      TRY.
          IF lt_fu_key IS NOT INITIAL.
            me->gs_new_tor-tor_srvmgr->retrieve( EXPORTING iv_node_key   = /scmtms/if_tor_c=>sc_node-root
                                                           it_key        = lt_fu_key
                                                 IMPORTING eo_message    = lo_message
                                                           et_data       = et_fu_root
                                                           et_failed_key = lt_failed ).
          ENDIF.
        CATCH cx_root.
      ENDTRY.

      me->prepare_message( EXPORTING io_message  = lo_message
                           IMPORTING et_messages = et_messages
                                     et_return   = et_return
                                     ev_result   = ev_result  ).

      CHECK ev_result NE if_fpm_constants=>gc_event_result-failed.

    ENDIF.

  ENDMETHOD.


  METHOD check_selection_sm_complete.

    FREE: et_messages, ev_result.

    CHECK ir_bukrs IS NOT INITIAL.

    " Inicia validações para empresas SIM
    LOOP AT it_root REFERENCE INTO DATA(ls_root).

* ---------------------------------------------------------------------------
* Verifica se código faz parte das empresas SIM
* ---------------------------------------------------------------------------
      LOOP AT it_item_tr INTO DATA(ls_item_tr) USING KEY parent_key WHERE parent_key    EQ ls_root->key
                                                                      AND erp_comp_code IN ir_bukrs.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

* ---------------------------------------------------------------------------
* Verifica se Motorista foi informado
* ---------------------------------------------------------------------------
      READ TABLE it_item_tr INTO ls_item_tr WITH KEY parent_key COMPONENTS parent_key = ls_root->key
                                                                           item_cat   = /scmtms/if_tor_const=>sc_tor_item_category-driver.
      IF sy-subrc NE 0.
        CLEAR ls_item_tr.
      ENDIF.

      IF ls_item_tr-res_id IS INITIAL.

        " OF &1 sem informação de Motorista.
        et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                  msgno       = 024
                                                  severity    = if_xo_const_message=>error
                                                  parameter_1 = |{ ls_root->tor_id ALPHA = OUT }| ) ).

        ev_result   = if_fpm_constants=>gc_event_result-failed.
      ENDIF.

* ---------------------------------------------------------------------------
* Verifica Transportadora
* ---------------------------------------------------------------------------
      IF ls_root->tspid IS INITIAL.

        " OF &1 sem informação de Transportadora.
        et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                  msgno       = 025
                                                  severity    = if_xo_const_message=>error
                                                  parameter_1 = |{ ls_root->tor_id ALPHA = OUT }| ) ).

        ev_result   = if_fpm_constants=>gc_event_result-failed.
      ENDIF.

* ---------------------------------------------------------------------------
* Verifica Lacres
* ---------------------------------------------------------------------------
      CLEAR ls_item_tr.
      LOOP AT it_item_tr INTO ls_item_tr USING KEY parent_key WHERE parent_key = ls_root->key
                                                                AND item_type  = gc_item_type-truc
                                                                AND item_cat   = /scmtms/if_tor_const=>sc_tor_item_category-av_item
                                                                AND zz_seal_number_total > 0.
        EXIT.
      ENDLOOP.

      DATA(lv_seal_number_total) = REDUCE ze_tm_seal_number_total( INIT lv_sum_ = 0 FOR ls_seal_ IN it_seal USING KEY root_key
                                                                   WHERE ( root_key = ls_root->key )
                                                                   NEXT lv_sum_ = lv_sum_ + 1 ).

      IF ls_item_tr-zz_seal_number_total > 0 AND lv_seal_number_total <= ls_item_tr-zz_seal_number_total.

        " OF &1 não possui todos os lacres preenchidos [&2/&3]
        et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                  msgno       = 026
                                                  severity    = if_xo_const_message=>error
                                                  parameter_1 = |{ ls_root->tor_id ALPHA = OUT }|
                                                  parameter_2 = |{ CONV string( lv_seal_number_total ) ALPHA = OUT }|
                                                  parameter_3 = |{ CONV string( ls_item_tr-zz_seal_number_total ) ALPHA = OUT }| ) ).

        ev_result   = if_fpm_constants=>gc_event_result-failed.
      ENDIF.

* ---------------------------------------------------------------------------
* Verifica Placas
* ---------------------------------------------------------------------------
      LOOP AT it_item_tr INTO ls_item_tr USING KEY parent_key WHERE parent_key = ls_root->key.

        CHECK ls_item_tr-platenumber IS INITIAL.

        " Cenário Truck
        IF ( ls_item_tr-item_type = gc_item_type-truc AND ls_item_tr-mtr = gc_mtr-trk )
        " Cenário Conjunto
        OR ( ls_item_tr-mtr = gc_mtr-conj AND ls_item_tr-ct_seq > 0 ).

          et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                    msgno       = SWITCH #( ls_item_tr-mtr
                                                                  WHEN gc_mtr-trk  THEN 027   " OF &1 sem a placa preenchida no Truck
                                                                  WHEN gc_mtr-carr THEN 028   " OF &1 sem a placa preenchida na Carreta
                                                                  WHEN gc_mtr-cav  THEN 029   " OF &1 sem a placa preenchida no Cavalo
                                                                  WHEN gc_mtr-conj THEN 030   " OF &1 sem a placa preenchida no Conjunto cavalo + carreta
                                                                  ELSE 027 )
                                                    severity    = if_xo_const_message=>error
                                                    parameter_1 = |{ ls_root->tor_id ALPHA = OUT }| ) ).

          ev_result   = if_fpm_constants=>gc_event_result-failed.

        ENDIF.

      ENDLOOP.

* ---------------------------------------------------------------------------
* Verifica Status das Unidades de Frete
* ---------------------------------------------------------------------------
      LOOP AT it_item_tr INTO ls_item_tr USING KEY parent_key WHERE parent_key = ls_root->key.

        " Busca pela associação com o nó filho (Unidade de Frete)
        LOOP AT it_child_item REFERENCE INTO DATA(ls_child_item) USING KEY item_parent_key WHERE item_parent_key = ls_item_tr-key.

          " Busca dados da Unidade de Frete
          READ TABLE it_fu_root REFERENCE INTO DATA(ls_fu_root) WITH TABLE KEY key = ls_child_item->fu_root_key.

          CHECK sy-subrc EQ 0.

          IF ls_fu_root->dlv_goods_mvmnt NE gc_dlv_goods_mvmnt_sts-completely_processed.

            et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                      msgno       = SWITCH #( ls_fu_root->dlv_goods_mvmnt
                                                                    WHEN gc_dlv_goods_mvmnt_sts-not_relevant        THEN 031   " OF &1 possui UF &2 com status não relevante.
                                                                    WHEN gc_dlv_goods_mvmnt_sts-not_processed       THEN 032   " OF &1 possui UF &2 com status não processado.
                                                                    WHEN gc_dlv_goods_mvmnt_sts-partially_processed THEN 033   " OF &1 possui UF &2 com status parcialmente processado.
                                                                    ELSE 031 )
                                                      severity    = if_xo_const_message=>error
                                                      parameter_1 = |{ ls_root->tor_id ALPHA = OUT }|
                                                      parameter_2 = |{ ls_fu_root->tor_id ALPHA = OUT }| ) ).

            ev_result   = if_fpm_constants=>gc_event_result-failed.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_selection_sm_partial.

    FREE: et_messages, ev_result.

    CHECK ir_bukrs IS NOT INITIAL.

    " Inicia validações para empresas DISTRIBUIDORA CHARRUA
    LOOP AT it_root REFERENCE INTO DATA(ls_root).

* ---------------------------------------------------------------------------
* Verifica se código faz parte das empresas DISTRIBUIDORA CHARRUA
* ---------------------------------------------------------------------------
      LOOP AT it_item_tr INTO DATA(ls_item_tr) USING KEY parent_key WHERE parent_key    EQ ls_root->key
                                                                      AND erp_comp_code IN ir_bukrs.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

* ---------------------------------------------------------------------------
* Verifica se Motorista foi informado
* ---------------------------------------------------------------------------
      READ TABLE it_item_tr INTO ls_item_tr WITH KEY parent_key COMPONENTS parent_key = ls_root->key
                                                                           item_cat   = /scmtms/if_tor_const=>sc_tor_item_category-driver.
      IF sy-subrc NE 0.
        CLEAR ls_item_tr.
      ENDIF.

      IF ls_item_tr-res_id IS INITIAL.

        " OF &1 sem informação de Motorista.
        et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                  msgno       = 024
                                                  severity    = if_xo_const_message=>error
                                                  parameter_1 = |{ ls_root->tor_id ALPHA = OUT }| ) ).

        ev_result   = if_fpm_constants=>gc_event_result-failed.
      ENDIF.

* ---------------------------------------------------------------------------
* Verifica Transportadora
* ---------------------------------------------------------------------------
      IF ls_root->tspid IS INITIAL.

        " OF &1 sem informação de Transportadora.
        et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                  msgno       = 025
                                                  severity    = if_xo_const_message=>error
                                                  parameter_1 = |{ ls_root->tor_id ALPHA = OUT }| ) ).

        ev_result   = if_fpm_constants=>gc_event_result-failed.
      ENDIF.

    ENDLOOP.

* ---------------------------------------------------------------------------
* Verifica Status das Unidades de Frete
* ---------------------------------------------------------------------------
    LOOP AT it_item_tr INTO ls_item_tr USING KEY parent_key WHERE parent_key = ls_root->key.

      " Busca pela associação com o nó filho (Unidade de Frete)
      LOOP AT it_child_item REFERENCE INTO DATA(ls_child_item) USING KEY item_parent_key WHERE item_parent_key = ls_item_tr-key.

        " Busca dados da Unidade de Frete
        READ TABLE it_fu_root REFERENCE INTO DATA(ls_fu_root) WITH TABLE KEY key = ls_child_item->fu_root_key.

        CHECK sy-subrc EQ 0.

        IF ls_fu_root->dlv_goods_mvmnt NE gc_dlv_goods_mvmnt_sts-completely_processed.

          et_messages = VALUE #( BASE et_messages ( msgid       = gc_message-id
                                                    msgno       = SWITCH #( ls_fu_root->dlv_goods_mvmnt
                                                                  WHEN gc_dlv_goods_mvmnt_sts-not_relevant        THEN 031   " OF &1 possui UF &2 com status não relevante.
                                                                  WHEN gc_dlv_goods_mvmnt_sts-not_processed       THEN 032   " OF &1 possui UF &2 com status não processado.
                                                                  WHEN gc_dlv_goods_mvmnt_sts-partially_processed THEN 033   " OF &1 possui UF &2 com status parcialmente processado.
                                                                  ELSE 031 )
                                                    severity    = if_xo_const_message=>error
                                                    parameter_1 = |{ ls_root->tor_id ALPHA = OUT }|
                                                    parameter_2 = |{ ls_fu_root->tor_id ALPHA = OUT }| ) ).

          ev_result   = if_fpm_constants=>gc_event_result-failed.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
