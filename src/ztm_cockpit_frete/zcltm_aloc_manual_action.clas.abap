CLASS zcltm_aloc_manual_action DEFINITION
  PUBLIC
  INHERITING FROM /scmtms/cl_ui_tcact_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scmtms/if_ui_tcact_main_wnd .

    CONSTANTS:
      BEGIN OF gc_stop_seq_pos,
        first TYPE /scmtms/stop_seq_pos VALUE 'F',
        last  TYPE /scmtms/stop_seq_pos VALUE 'L',
      END OF gc_stop_seq_pos,

      BEGIN OF gc_stop_cat,
        inbound  TYPE /scmtms/stop_category VALUE 'I',
        outbound TYPE /scmtms/stop_category VALUE 'O',
      END OF gc_stop_cat.

    METHODS convert_material_unit
      IMPORTING
        !is_item      TYPE /scmtms/s_tor_item_tr_k
        !is_fu_item   TYPE /scmtms/s_tor_item_tr_k
      RETURNING
        VALUE(rv_qty) TYPE bstmg .
    METHODS receive_results
      IMPORTING
        !p_task TYPE clike .

    METHODS /scmtms/if_ui_tcact~get_action_ids
        REDEFINITION.

    METHODS insert_stop_for_fo
      IMPORTING
        !io_tor_srvmgr TYPE REF TO /bobf/if_tra_service_manager OPTIONAL
        !it_ct_key     TYPE /bobf/t_frw_key
        !it_fu_key     TYPE /bobf/t_frw_key
      EXPORTING
        !et_messages   TYPE fpmgb_t_messages.

    METHODS call_rfc_insert_stop_for_fo
      IMPORTING
        !it_ct_key  TYPE /bobf/t_frw_key
        !it_fu_key  TYPE /bobf/t_frw_key
      EXPORTING
        et_messages TYPE fpmgb_t_messages.

    METHODS update_tbo
      IMPORTING
        io_change   TYPE REF TO /bobf/if_tra_change OPTIONAL
        io_message  TYPE REF TO /bobf/if_frw_message OPTIONAL
      EXPORTING
        et_return   TYPE bapiret2_t
        et_messages TYPE fpmgb_t_messages.

protected section.

  constants GC_MY_ACTION_ID type FPM_EVENT_ID value 'ZTB_ALOC_MANUAL_ACTION' ##NO_TEXT.

  methods SPLIT_FU .
  methods ASSIGN_FU_TO_FO
    returning
      value(RT_MESSAGES) type FPMGB_T_MESSAGES .
  methods PREPARE_MAIN_TABLE
    importing
      !IT_COMPARTIMENTOS type /SCMTMS/T_TOR_ITEM_TR_K
      !IT_FU type /SCMTMS/T_TOR_ROOT_K .
  methods CHECK_STATUS_DELIVERY
    importing
      !IT_FU_ROOT type /SCMTMS/T_TOR_ROOT_K
    returning
      value(RT_MESSAGES) type FPMGB_T_MESSAGES .
  methods CHECK_SELECTION_MAIN
    importing
      !IT_FU_ROOT type /SCMTMS/T_TOR_ROOT_K
      !IT_COMPARTIMENTOS_KEYS type /BOBF/T_FRW_KEY
    exporting
      !EV_RESULT type FPM_EVENT_RESULT
      !ET_MESSAGES type FPMGB_T_MESSAGES .
  methods CHECK_SELECTION_SPECIAL
    importing
      !IT_FU_ROOT type /SCMTMS/T_TOR_ROOT_K
      !IT_COMPARTIMENTOS_KEYS type /BOBF/T_FRW_KEY
    exporting
      !EV_RESULT type FPM_EVENT_RESULT
      !ET_MESSAGES type FPMGB_T_MESSAGES .

  methods CHECK_SELECTION
    redefinition .
  methods EXECUTE
    redefinition .
  methods PREPARE
    redefinition .
  PRIVATE SECTION.

    CONSTANTS gc_memory_id TYPE abapdocu_buff_srtfd VALUE 'ZTM_EQUI_INDEX' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF gc_mgs,
        classe  TYPE symsgid VALUE 'ZTM_COCKPIT_FRETE',
        num_004 TYPE symsgno VALUE '004',
        num_005 TYPE symsgno VALUE '005',
        num_006 TYPE symsgno VALUE '006',
        num_007 TYPE symsgno VALUE '007',
        num_008 TYPE symsgno VALUE '008',
        num_016 TYPE symsgno VALUE '016',
        num_017 TYPE symsgno VALUE '017',
        num_018 TYPE symsgno VALUE '018',
        num_019 TYPE symsgno VALUE '019',
      END OF gc_mgs .
    CONSTANTS:
      BEGIN OF gc_event_result,
        ok     TYPE fpm_event_result VALUE 'OK',
        failed TYPE fpm_event_result VALUE 'FAILED',
      END OF gc_event_result .
    CONSTANTS:
      BEGIN OF gc_empresa,
        modulo TYPE ze_param_modulo VALUE 'TM',
        chave1 TYPE ze_param_chave1 VALUE 'ALOC_EMPRESA',
      END OF gc_empresa.
    DATA gt_main TYPE zctgtm_aloc_manual .
    DATA gv_result TYPE flag .
    DATA gt_return TYPE bapiret2_t.
ENDCLASS.



CLASS ZCLTM_ALOC_MANUAL_ACTION IMPLEMENTATION.


  METHOD /scmtms/if_ui_tcact~get_action_ids.
    rt_action_ids = VALUE #( (  gc_my_action_id ) ).
  ENDMETHOD.


  METHOD check_selection.

    " Tabelas internas
    DATA: lt_compartimentos_keys TYPE /bobf/t_frw_key,
          lt_fu_root             TYPE /scmtms/t_tor_root_k,
          lt_tor_item            TYPE /scmtms/t_tor_item_tr_k,
          lt_fu_item             TYPE /scmtms/t_tor_item_tr_k.

    " Variáveis
    DATA: lv_compartimentos      TYPE char50.

    " Ranges
    DATA: lr_empresa TYPE RANGE OF char4.

    ev_result = gc_event_result-ok.

    " Import de dados da classe : /SCMTMS/CL_UI_TBI_TOR_ITEM_EQI
    "                             /SCMTMS/IF_UI_TBI_FPM~ADAPT_EVENT
    "                             ENHANCEMENT 1  ZEI_TM_EQUI_SELECTED
    IMPORT lt_compartimentos_keys TO lt_compartimentos_keys FROM MEMORY ID gc_memory_id.

**********************************************************************
* BEGIN OF INSERT - 07.03.2024 - RDSP - GAPTM37
    mo_tor_srvmgr->retrieve(
      EXPORTING
        iv_node_key = /scmtms/if_tor_c=>sc_node-item_tr                         " Node
        it_key      = lt_compartimentos_keys                                    " Key Table
      IMPORTING
        et_data     = lt_tor_item ).

    CHECK lt_tor_item IS NOT INITIAL.

    mo_tor_srvmgr->retrieve(
      EXPORTING
        iv_node_key = /scmtms/if_tor_c=>sc_node-root                            " Node
        it_key      = ms_selection-freight_unit-root                            " Key Table
      IMPORTING
        et_data     = lt_fu_root ).

    CHECK lt_fu_root IS NOT INITIAL.

    mo_tor_srvmgr->retrieve_by_association(
      EXPORTING
        iv_node_key    = /scmtms/if_tor_c=>sc_node-root                         " Node
        it_key         = ms_selection-freight_unit-root                         " Key Table
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr          " Association
        iv_fill_data   = abap_true
      IMPORTING
        et_data        = lt_fu_item ).

    TRY.

        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = gc_empresa-modulo
            iv_chave1 = gc_empresa-chave1
          IMPORTING
            et_range  = lr_empresa ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

    DATA(lt_empresa) = VALUE t_bukrs( FOR <fs_emp> IN lt_fu_item  WHERE
                                    ( item_cat = /scmtms/if_tor_const=>sc_itemcat_prd )
                                    ( <fs_emp>-erp_comp_code ) ).

    SORT lt_empresa.
    DELETE ADJACENT DUPLICATES FROM lt_empresa COMPARING ALL FIELDS.

    CHECK lt_empresa IS NOT INITIAL.

    IF lt_empresa[ 1 ] IN lr_empresa.

      check_selection_special( EXPORTING it_fu_root             = lt_fu_root
                                         it_compartimentos_keys = lt_compartimentos_keys
                               IMPORTING ev_result              = ev_result
                                         et_messages            = et_messages ).              " FPMGB Messages (T100 & Plaintext)

    ELSE.

      check_selection_main( EXPORTING it_fu_root             = lt_fu_root                     " Root Node
                                      it_compartimentos_keys = lt_compartimentos_keys         " Key Table
                            IMPORTING ev_result              = ev_result
                                      et_messages            = et_messages ).                 " FPMGB Messages (T100 & Plaintext)

    ENDIF.


**********************************************************************
* END OF INSERT - 07.03.2024 - RDSP - GAPTM37

**********************************************************************
* BEGIN OF DELETION - 07.03.2024 - RDSP - GAPTM37
*    IF lt_compartimentos_keys IS INITIAL.
*      " Compartimento não selecionado
*      APPEND INITIAL LINE TO et_messages ASSIGNING FIELD-SYMBOL(<fs_msg>).
*      <fs_msg>-msgid    = gc_mgs-classe.
*      <fs_msg>-msgno    = gc_mgs-num_004.
*      <fs_msg>-severity = if_xo_const_message=>error.
*      ev_result         = gc_event_result-failed.
*      RETURN.
*    ENDIF.
*
*    IF ms_selection-freight_unit-root IS INITIAL.
*      " Selecionar uma unidade de frete
*      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
*      <fs_msg>-msgid    = gc_mgs-classe.
*      <fs_msg>-msgno    = gc_mgs-num_005.
*      <fs_msg>-severity = if_xo_const_message=>error.
*      ev_result         = gc_event_result-failed.
*      RETURN.
*    ELSEIF lines( ms_selection-freight_unit-root ) > 1.
*      " Selecionar apenas uma unidade de Frete
*      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
*      <fs_msg>-msgid    = gc_mgs-classe.
*      <fs_msg>-msgno    = gc_mgs-num_006.
*      <fs_msg>-severity = if_xo_const_message=>error.
*      ev_result         = gc_event_result-failed.
*      RETURN.
*    ENDIF.
*
**    mo_tor_srvmgr->retrieve(
**      EXPORTING
**        iv_node_key = /scmtms/if_tor_c=>sc_node-item_tr                         " Node
**        it_key      = lt_compartimentos_keys                                    " Key Table
**      IMPORTING
**        et_data     = lt_tor_item ).
**
**    CHECK lt_tor_item IS NOT INITIAL.
**
**    mo_tor_srvmgr->retrieve(
**      EXPORTING
**        iv_node_key = /scmtms/if_tor_c=>sc_node-root                            " Node
**        it_key      = ms_selection-freight_unit-root                            " Key Table
**      IMPORTING
**        et_data     = lt_fu_root ).
**
**    CHECK lt_fu_root IS NOT INITIAL.
***********************************************************************
** BEGIN OF INSERT - 15.02.2024 - RDSP - GAPTM15
*    DATA(lt_messages) = check_status_delivery( it_fu_root = lt_fu_root ). "#EC CI_CONV_OK
*
*    IF lt_messages IS NOT INITIAL.
*      et_messages = VALUE #( FOR <fs_messages> IN lt_messages
*                           ( msgid        = <fs_messages>-msgid
*                             msgno        = <fs_messages>-msgno
*                             severity     = <fs_messages>-severity
*                             parameter_1  = <fs_messages>-parameter_1 ) ).
*      ev_result   = gc_event_result-failed.
*      RETURN.
*    ENDIF.
** END OF INSERT - 15.02.2024 - RDSP - GAPTM15
***********************************************************************
*    mo_tor_srvmgr->retrieve_by_association(
*      EXPORTING
*        iv_node_key    = /scmtms/if_tor_c=>sc_node-root                         " Node
*        it_key         = ms_selection-freight_unit-root                         " Key Table
*        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr          " Association
*        iv_fill_data   = abap_true
*      IMPORTING
*        et_data        = lt_fu_item ).
*
*    CHECK lt_fu_item IS NOT INITIAL.
*
*    IF line_exists( lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ). "#EC CI_SORTSEQ
*      DATA(ls_fu_item)  = lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]. "#EC CI_SORTSEQ
*      DATA(ls_tor_item)  = lt_tor_item[ 1 ].
*    ELSE.
*      RETURN.
*    ENDIF.
*
*    " Soma as capacidades dos compartimentos
*    DATA(lv_cap_compart) = REDUCE /scmtms/qua_gro_vol_val( INIT lv_sun TYPE /scmtms/qua_gro_vol_val
*                                                           FOR <fs_sun> IN lt_tor_item
*                                                           NEXT lv_sun = lv_sun + <fs_sun>-gro_vol_valcap ).
*
*    " Converte qtde da FU para a unidade de medida do compartimento
*    DATA(lv_qty_fu) = convert_material_unit( is_item    = ls_tor_item           " Transportation Order Item
*                                             is_fu_item = ls_fu_item ).         " Transportation Order FU Item
*
*    " Concatena as descrições dos compartimentos
*    CLEAR lv_compartimentos.
*    LOOP AT lt_tor_item ASSIGNING FIELD-SYMBOL(<fs_compart>).
*      IF lv_compartimentos IS INITIAL.
*        lv_compartimentos = <fs_compart>-item_descr.
*      ELSE.
*        lv_compartimentos = lv_compartimentos && space && <fs_compart>-item_descr.
*      ENDIF.
*    ENDLOOP.
*
*    " Qtde Fu menor que capacidade do compartimento
**    IF lv_qty_fu < ls_tor_item-gro_vol_valcap.
*    IF lv_qty_fu < lv_cap_compart.
*
*      " Quantidade da Unidade de Frete &1 é menor que compartimentos &2
*      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
*      <fs_msg>-msgid       = gc_mgs-classe.
*      <fs_msg>-msgno       = gc_mgs-num_007.
*      <fs_msg>-parameter_1 = |{ lt_fu_root[ 1 ]-tor_id ALPHA = OUT }|.
*      <fs_msg>-parameter_2 = lv_compartimentos.
*      <fs_msg>-severity    = if_xo_const_message=>error.
*      ev_result            = gc_event_result-failed.
*      RETURN.
*
*      " Qtde Fu maior que capacidade do compartimento
**    ELSEIF lv_qty_fu > ls_tor_item-gro_vol_valcap.
*    ELSEIF lv_qty_fu > lv_cap_compart.
*
*      " Quantidade da Unidade de Frete &1 é maior que compartimentos &2
*      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
*      <fs_msg>-msgid       = gc_mgs-classe.
*      <fs_msg>-msgno       = gc_mgs-num_008.
*      <fs_msg>-parameter_1 = |{ lt_fu_root[ 1 ]-tor_id ALPHA = OUT }|.
*      <fs_msg>-parameter_2 = lv_compartimentos.
*      <fs_msg>-severity    = if_xo_const_message=>error.
*      ev_result            = gc_event_result-failed.
*      RETURN.
*
*    ENDIF.
**********************************************************************
* END OF DELETION - 07.03.2024 - RDSP - GAPTM37

  ENDMETHOD.


  METHOD execute.

    " Tabelas internas
    DATA: lt_compartimentos_keys TYPE /bobf/t_frw_key,
          lt_fu_root             TYPE /scmtms/t_tor_root_k,
          lt_tor_root            TYPE /scmtms/t_tor_root_k,
          lt_tor_item            TYPE /scmtms/t_tor_item_tr_k,
          lt_return              TYPE bapiret2_tab.

    " Variaveis
    DATA: lv_clear_selection     TYPE boole_d.

    CLEAR: gt_main,
           lt_compartimentos_keys,
           lt_fu_root,
           lt_tor_item.

    " Import de dados da classe : /SCMTMS/CL_UI_TBI_TOR_ITEM_EQI
    "                             /SCMTMS/IF_UI_TBI_FPM~ADAPT_EVENT
    "                             ENHANCEMENT 1  ZEI_TM_EQUI_SELECTED
    IMPORT lt_compartimentos_keys TO lt_compartimentos_keys FROM MEMORY ID gc_memory_id.
    CHECK lt_compartimentos_keys IS NOT INITIAL.

    TRY.

        mo_tor_srvmgr->retrieve(
          EXPORTING
            iv_node_key = /scmtms/if_tor_c=>sc_node-root                        " Node
            it_key      = ms_selection-freight_unit-root                        " Key Table
          IMPORTING
            et_data     = lt_fu_root ).

        CHECK lt_fu_root IS NOT INITIAL.

        mo_tor_srvmgr->retrieve(
          EXPORTING
            iv_node_key = /scmtms/if_tor_c=>sc_node-item_tr                     " Node
            it_key      = lt_compartimentos_keys                                " Key Table
          IMPORTING
            et_data     = lt_tor_item ).

        CHECK lt_tor_item IS NOT INITIAL.

        " Prepara tabela principal para Split/Asociação de UFs
        prepare_main_table( it_compartimentos = lt_tor_item                     " Transportation Order Item
                            it_fu             = lt_fu_root ).                   " Root Node

        CHECK gt_main IS NOT INITIAL.

        " Split de Unidade de Frete
        split_fu( ).

        " Associação de Unidade de Frete na Ordem de Frete
        et_messages = CORRESPONDING #( DEEP assign_fu_to_fo( ) ). "#EC CI_CONV_OK

        IF NOT line_exists( et_messages[ severity = 'E' ] ). "#EC CI_STDSEQ
          DELETE et_messages WHERE severity = if_xo_const_message=>warning. "#EC CI_STDSEQ
          DELETE et_messages WHERE severity = if_xo_const_message=>info. "#EC CI_STDSEQ
        ENDIF.

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        RETURN.
    ENDTRY.

    me->/scmtms/if_ui_tcact~cleanup( ).

  ENDMETHOD.


  METHOD /scmtms/if_ui_tcact_main_wnd~process_action.

    RETURN.

  ENDMETHOD.


  METHOD prepare.

    super->prepare( EXPORTING io_controller          = io_controller          " /BOFU/IF_FBI_CONTROLLER_NEW
                              io_log                 = io_log                 " Trace / Logging: Trace class
                              io_mp_tc               = io_mp_tc               " Execute Planning Function
                              io_tbo_srvmgr          = io_tbo_srvmgr          " Service Manager for tBO /SCMTMS/PLN
                              io_tor_srvmgr          = io_tor_srvmgr          " Service Manager for BO /SCMTMS/TOR
                              io_event               = io_event               " Event instance
                              iv_block_mw_processing = iv_block_mw_processing " Indicator: Block the processing in Main Window
                    IMPORTING ev_result              = ev_result              " Result of processing
                              et_messages            = et_messages ).         " List with occured messages

  ENDMETHOD.


  METHOD convert_material_unit.

    DATA: lv_matnr  TYPE matnr,
          lv_in_me  TYPE meins,
          lv_out_me TYPE meins,
          lv_menge  TYPE bstmg.

    lv_matnr   = is_fu_item-product_id.
    lv_in_me   = is_fu_item-gro_vol_uni.
    lv_out_me  = is_item-gro_vol_unicap.
    lv_menge   = is_fu_item-gro_vol_val.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = lv_matnr
        i_in_me              = lv_in_me
        i_out_me             = lv_out_me
        i_menge              = lv_menge
      IMPORTING
        e_menge              = rv_qty
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD split_fu.

    DATA: lt_param   TYPE /scmtms/t_tor_root_a_doc_split,
          lt_fu_root TYPE /scmtms/t_tor_root_k,
          lt_fu_item TYPE /scmtms/t_tor_item_tr_k,
          lt_key     TYPE /bobf/t_frw_key.

    DATA: lv_fu_root TYPE /bobf/conf_key.

    CLEAR: gv_result.
    CALL FUNCTION 'ZFMTM_ALOC_MANUAL_SPLIT_FU'
      STARTING NEW TASK 'ALOC_MANUAL_SPLIT_FU'
      CALLING receive_results ON END OF TASK
      EXPORTING
        it_main = gt_main.

    WAIT UNTIL gv_result IS NOT INITIAL UP TO 15 SECONDS.

  ENDMETHOD.


  METHOD assign_fu_to_fo.

    LOOP AT gt_main ASSIGNING FIELD-SYMBOL(<fs_main>).

      " Adiciona novas paradas quando necessário
      me->insert_stop_for_fo( EXPORTING io_tor_srvmgr = mo_tor_srvmgr
                                        it_ct_key     = VALUE #( ( key = <fs_main>-compartimento ) )
                                        it_fu_key     = VALUE #( ( key = <fs_main>-unid_frete ) )
                              IMPORTING et_messages = rt_messages ).

      IF rt_messages[] IS NOT INITIAL.
        RETURN.
      ENDIF.

      DATA(lo_add_fu_param) = NEW /scmtms/s_tor_a_add_fu_item( string = <fs_main>-tor_id ).

      " Associa a Unidade de Frete na Ordem de Frete
      mo_tor_srvmgr->do_action( EXPORTING iv_act_key    = /scmtms/if_tor_c=>sc_action-item_tr-add_fu_by_fuid
                                          it_key        = VALUE #( ( key = <fs_main>-compartimento ) )
                                          is_parameters = lo_add_fu_param
                                IMPORTING eo_change     = DATA(lo_change)
                                          eo_message    = DATA(lo_message) ).

      me->update_tbo( EXPORTING io_change   = lo_change
                                io_message  = lo_message
                      IMPORTING et_messages = DATA(lt_messages) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_main_table.

    DATA: lt_fu_item             TYPE /scmtms/t_tor_item_tr_k.

    CHECK it_compartimentos IS NOT INITIAL.

    mo_tor_srvmgr->retrieve_by_association(
      EXPORTING
        iv_node_key    = /scmtms/if_tor_c=>sc_node-root                         " Node
        it_key         = CORRESPONDING #( it_fu MAPPING key = key )             " Key Table
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr          " Association
        iv_fill_data   = abap_true
      IMPORTING
        et_data        = lt_fu_item ).

    CHECK lt_fu_item IS NOT INITIAL.
    IF line_exists( lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ). "#EC CI_SORTSEQ
      DATA(ls_fu_item)  = lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]. "#EC CI_SORTSEQ
      DATA(ls_tor_item)  = it_compartimentos[ 1 ].
    ELSE.
      RETURN.
    ENDIF.

    " Converte qtde da FU para a unidade de medida do compartimento
    DATA(lv_qty_fu) = convert_material_unit( is_item    = ls_tor_item           " Transportation Order Item
                                             is_fu_item = ls_fu_item ).         " Transportation Order FU Item

    IF lines( it_fu ) > 1.

      LOOP AT it_fu ASSIGNING FIELD-SYMBOL(<fs_fu>).
        APPEND INITIAL LINE TO gt_main ASSIGNING FIELD-SYMBOL(<fs_main>).

*        IF sy-tabix EQ 1.
          <fs_main>-unid_frete = <fs_fu>-key.
          <fs_main>-tor_id     = <fs_fu>-tor_id.
*        ENDIF.

        DATA(ls_compart) = VALUE #( it_compartimentos[ 1 ] OPTIONAL ).
        <fs_main>-compartimento = ls_compart-key.
        <fs_main>-capac_compart = ls_compart-gro_vol_valcap.

*        IF lv_qty_fu EQ ls_compart-gro_vol_valcap.
*          CLEAR: <fs_main>-capac_split.
*        ELSE.
*          <fs_main>-capac_split      = lv_qty_fu = lv_qty_fu - <fs_tor_item>-gro_vol_valcap.
*          <fs_main>-capac_split_unid = <fs_tor_item>-gro_vol_unicap.
*        ENDIF.
      ENDLOOP.

    ELSE.

      DATA(lv_compart) = lines( it_compartimentos ).

      LOOP AT it_compartimentos ASSIGNING FIELD-SYMBOL(<fs_tor_item>).
        APPEND INITIAL LINE TO gt_main ASSIGNING <fs_main>.

        IF sy-tabix EQ 1.
          <fs_main>-unid_frete = it_fu[ 1 ]-key.
          <fs_main>-tor_id     = it_fu[ 1 ]-tor_id.
        ENDIF.

        <fs_main>-compartimento = <fs_tor_item>-key.
        <fs_main>-capac_compart = <fs_tor_item>-gro_vol_valcap.

        IF lv_compart = 1.
          CONTINUE.
        ENDIF.

        IF lv_qty_fu LE <fs_tor_item>-gro_vol_valcap.
          CLEAR: <fs_main>-capac_split.
        ELSE.
          <fs_main>-capac_split      = lv_qty_fu = lv_qty_fu - <fs_tor_item>-gro_vol_valcap.
          <fs_main>-capac_split_unid = <fs_tor_item>-gro_vol_unicap.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD receive_results.

    CASE p_task.

      WHEN 'ALOC_MANUAL_SPLIT_FU'.

        RECEIVE RESULTS FROM FUNCTION 'ZFMTM_ALOC_MANUAL_SPLIT_FU'
          IMPORTING et_main   = gt_main
                    ev_return = gv_result.

      WHEN 'ALOC_MANUAL_INSERT_STOP'.

        RECEIVE RESULTS FROM FUNCTION 'ZFMTM_ALOC_MANUAL_SPLIT_FU'
          IMPORTING et_return   = gt_return.

    ENDCASE.

  ENDMETHOD.


  METHOD insert_stop_for_fo.

    DATA: lt_fo_root      TYPE /scmtms/t_tor_root_k,
          lt_fo_stop      TYPE /scmtms/t_tor_stop_k,
          lt_fo_stop_succ TYPE /scmtms/t_tor_stop_succ_k,

          lt_fu_root      TYPE /scmtms/t_tor_root_k,
          lt_fu_stop      TYPE /scmtms/t_tor_stop_k,

          lt_fo_stop_new  TYPE STANDARD TABLE OF /scmtms/s_tor_stop_k.

    FREE: et_messages.

    IF io_tor_srvmgr IS BOUND.
      DATA(lo_tor_srvmgr) = io_tor_srvmgr.
    ELSE.
      lo_tor_srvmgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera dados da ordem de frete
* ---------------------------------------------------------------------------
    lo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-item_tr
                                                      it_key         = it_ct_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-item_tr-to_root
                                                      iv_fill_data   = abap_true
                                            IMPORTING et_data        = lt_fo_root
                                                      et_target_key  = DATA(lt_fo_key) ).

    " Recupera ordem de frete (será sempre uma que iremos associar)
    DATA(ls_fo_root)   = VALUE #( lt_fo_root[ 1 ] OPTIONAL ).

    CHECK ls_fo_root IS NOT INITIAL.

* ---------------------------------------------------------------------------
* Recupera paradas da ordem de frete
* ---------------------------------------------------------------------------
* OBS: Somente prosseguir se existe paradas na Ordem de Frete. Quando a OF não
*      possui paradas, o standard insere automaticamente com a origem e destino
*      da primeira UF associada.
* ---------------------------------------------------------------------------
    lo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                      it_key         = lt_fo_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-stop
                                                      iv_fill_data   = abap_true
                                            IMPORTING et_data        = lt_fo_stop ).

    " Recupera primeira parada de origem da OF
    DATA(ls_fo_stop_f) = VALUE #( lt_fo_stop[ KEY stop_seq_pos
                                              root_key     = ls_fo_root-key
                                              stop_seq_pos = gc_stop_seq_pos-first ] OPTIONAL ).
    CHECK ls_fo_stop_f IS NOT INITIAL.
    CHECK ls_fo_stop_f-log_locid IS NOT INITIAL.

    " Recupera última parada de destino da OF
    DATA(ls_fo_stop_l) = VALUE #( lt_fo_stop[ KEY stop_seq_pos
                                              root_key     = ls_fo_root-key
                                              stop_seq_pos = gc_stop_seq_pos-last ] OPTIONAL ).
    CHECK ls_fo_stop_l IS NOT INITIAL.

* ---------------------------------------------------------------------------
* Recupera sequencia de paradas da ordem de frete
* ---------------------------------------------------------------------------
    lo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                      it_key         = lt_fo_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-stop_succ
                                                      iv_fill_data   = abap_true
                                            IMPORTING et_data        = lt_fo_stop_succ ).

* ---------------------------------------------------------------------------
* Recupera dados da unidade de frete
* ---------------------------------------------------------------------------
    lo_tor_srvmgr->retrieve( EXPORTING iv_node_key = /scmtms/if_tor_c=>sc_node-root
                                       it_key      = it_fu_key
                             IMPORTING et_data     = lt_fu_root ).


* ---------------------------------------------------------------------------
* Recupera primeira e última parada da unidade de frete
* ---------------------------------------------------------------------------
    lo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                      it_key         = it_fu_key
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-stop_first_and_last
                                                      iv_fill_data   = abap_true
                                            IMPORTING et_data        = lt_fu_stop ).

* ---------------------------------------------------------------------------
* Valida se a unidade de frete pode ser inserida na ordem de frete
* ---------------------------------------------------------------------------
    LOOP AT it_fu_key REFERENCE INTO DATA(ls_fu_key).

      " Recupera unidade de frete
      DATA(ls_fu_root)   = VALUE #( lt_fu_root[ key = ls_fu_key->key ] OPTIONAL ).

      CHECK ls_fu_root IS NOT INITIAL.

      " Recupera primeira para de origem da UF
      DATA(ls_fu_stop_f) = VALUE #( lt_fu_stop[ KEY stop_seq_pos
                                                root_key     = ls_fu_key->key
                                                stop_seq_pos = gc_stop_seq_pos-first ] OPTIONAL ).
      CHECK ls_fu_stop_f IS NOT INITIAL.

      " Recupera última para de destino da UF
      DATA(ls_fu_stop_l) = VALUE #( lt_fu_stop[ KEY stop_seq_pos
                                                root_key     = ls_fu_key->key
                                                stop_seq_pos = gc_stop_seq_pos-last ] OPTIONAL ).
      CHECK ls_fu_stop_l IS NOT INITIAL.

      " Valida se as unidades gerenciais de origem da UF e OF são a mesma
      IF ls_fu_stop_f-log_locid NE ls_fo_stop_f-log_locid.

        " UF &1 possui unidade gerencial de origem diferente da OF &2.
        et_messages = VALUE #( BASE et_messages ( msgid      = 'ZTM_COCKPIT_FRETE'
                                                 msgno       = '015'
                                                 severity    = if_xo_const_message=>error
                                                 parameter_1 = |{ ls_fu_root-tor_id ALPHA = OUT }|
                                                 parameter_2 = |{ ls_fo_root-tor_id ALPHA = OUT }| ) ).

        CONTINUE.
      ENDIF.

      " Verifica se existe alguma parada com a unidade gerencial da UF.
      " Caso não exista, devemos adicionar uma nova parada na OF.
      IF NOT line_exists( lt_fo_stop[ KEY log_loc_uuid
                                      log_loc_uuid = ls_fu_stop_l-log_loc_uuid ] ).

        INSERT ls_fu_stop_l INTO TABLE lt_fo_stop_new[].

      ENDIF.

    ENDLOOP.

    IF et_messages[] IS NOT INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara para inserir as novas paradas com suas unidades gerenciais
* ---------------------------------------------------------------------------
    SORT lt_fo_stop_new BY log_loc_uuid log_locid.
    DELETE ADJACENT DUPLICATES FROM lt_fo_stop_new COMPARING log_loc_uuid log_locid.

    LOOP AT lt_fo_stop_new REFERENCE INTO DATA(ls_fo_stop_new).

      " Recupera última parada de destino da OF
      ls_fo_stop_l = VALUE #( lt_fo_stop[ KEY stop_seq_pos
                                          root_key     = ls_fo_root-key
                                          stop_seq_pos = gc_stop_seq_pos-last ] OPTIONAL ).
      CHECK ls_fo_stop_l IS NOT INITIAL.

      " Recupera Sucessor da última parada. É a partir dele que vamos inserir uma nova última parada.
      DATA(ls_fo_stop_succ) = VALUE #( lt_fo_stop_succ[ KEY succ_stop_key
                                                        succ_stop_key  = ls_fo_stop_l-key ] OPTIONAL ).
      CHECK ls_fo_stop_succ IS NOT INITIAL.

      " Prepara a nova parada e unidade gerencial que será inserida
      DATA(lo_action_param_loc_info) = NEW /scmtms/s_param_loc_info( log_loc_uuid = ls_fo_stop_new->log_loc_uuid
                                                                     log_locid    = ls_fo_stop_new->log_locid ).

      " Insere nova parada de destino.
      lo_tor_srvmgr->do_action( EXPORTING iv_act_key    = /scmtms/if_tor_c=>sc_action-stop_successor-insert_after
                                          it_key        = VALUE #( ( key = ls_fo_stop_succ-key ) )
                                          is_parameters = lo_action_param_loc_info
                                IMPORTING eo_change     = DATA(lo_change)
                                          eo_message    = DATA(lo_message) ).

      me->update_tbo( EXPORTING " io_change   = lo_change
                                io_message  = lo_message
                      IMPORTING et_messages = DATA(lt_messages) ).

      IF line_exists( lt_messages[ severity = if_xo_const_message=>error ] ).
        " Em caso de erro, abortar processo
        et_messages = VALUE #( BASE et_messages FOR ls_messages_ IN lt_messages WHERE ( severity = if_xo_const_message=>error ) ( ls_messages_ ) ).
        RETURN.
      ENDIF.

* ---------------------------------------------------------------------------
* Caso seja necessário inserir múltiplas UGs, devemos buscar novamente a nova
* ordem das paradas
* ---------------------------------------------------------------------------
      IF lines( lt_fo_stop_new ) > 1.

* ---------------------------------------------------------------------------
* Recupera paradas da ordem de frete
* ---------------------------------------------------------------------------
        lo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                          it_key         = lt_fo_key
                                                          iv_association = /scmtms/if_tor_c=>sc_association-root-stop
                                                          iv_fill_data   = abap_true
                                                IMPORTING et_data        = lt_fo_stop ).

* ---------------------------------------------------------------------------
* Recupera sequencia de paradas da ordem de frete
* ---------------------------------------------------------------------------
        lo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                                          it_key         = lt_fo_key
                                                          iv_association = /scmtms/if_tor_c=>sc_association-root-stop_succ
                                                          iv_fill_data   = abap_true
                                                IMPORTING et_data        = lt_fo_stop_succ ).


      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD update_tbo.

    FREE: et_return, et_messages.

    IF io_change IS BOUND.

      CALL METHOD /scmtms/cl_pln_tbo_factory=>update_tbo_from_changenot "#EC CI_SEL_NESTED
        EXPORTING
          io_change        = io_change
        IMPORTING
          eo_change_tr_tbo = DATA(lo_change_pln).

      " attention: add TBO - changes ... otherwise data will not be displayed in TC
      IF io_change IS BOUND AND lo_change_pln IS BOUND.
        io_change->merge( EXPORTING io_change = lo_change_pln ).
      ENDIF.

      " Dispatch changes and messages
      IF io_change  IS BOUND OR io_message IS BOUND.

        mo_controller->/bofu/if_fbi_controller~post_syncup_data( iv_bo_key  = /scmtms/if_pln_c=>sc_bo_key
                                                                 io_change  = io_change
                                                                 io_message = io_message ).
      ENDIF.

    ENDIF.

    IF io_message IS BOUND.

      /scmtms/cl_common_helper=>msg_convert_bopf_2_bapiret2( EXPORTING io_message  = io_message
                                                             CHANGING  ct_bapiret2 = et_return ).

      et_messages = VALUE #( BASE et_messages FOR ls_return_ IN et_return
                           ( msgid       = ls_return_-id
                             msgno       = ls_return_-number
                             severity    = ls_return_-type
                             parameter_1 = ls_return_-message_v1
                             parameter_2 = ls_return_-message_v2
                             parameter_3 = ls_return_-message_v3
                             parameter_4 = ls_return_-message_v4 ) ).

    ENDIF.

  ENDMETHOD.


  METHOD call_rfc_insert_stop_for_fo.

    FREE: gt_return, gv_result, et_messages.

    CALL FUNCTION 'ZFMTM_ALOC_MANUAL_INSERT_STOP'
      STARTING NEW TASK 'ALOC_MANUAL_INSERT_STOP'
      CALLING receive_results ON END OF TASK
      EXPORTING
        it_ct_key = it_ct_key
        it_fu_key = it_fu_key.

    WAIT UNTIL gv_result IS NOT INITIAL UP TO 15 SECONDS.


    et_messages = VALUE #( BASE et_messages FOR ls_return_ IN gt_return
                         ( msgid       = ls_return_-id
                           msgno       = ls_return_-number
                           severity    = ls_return_-type
                           parameter_1 = ls_return_-message_v1
                           parameter_2 = ls_return_-message_v2
                           parameter_3 = ls_return_-message_v3
                           parameter_4 = ls_return_-message_v4 ) ).

  ENDMETHOD.


  METHOD check_status_delivery.

    "
    DATA: lt_doc_ref TYPE /scmtms/t_tor_docref_k.

    DATA: lr_remessa TYPE RANGE OF vbeln_vl.

    TRY.
        mo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root                                   " Node
                                                          it_key         = CORRESPONDING #( it_fu_root MAPPING key = key )                  " Key Table
                                                          iv_association = /scmtms/if_tor_c=>sc_association-root-docreference               " Association
                                                          iv_fill_data   = abap_true                                                        " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
                                                IMPORTING et_data        = lt_doc_ref ).

        CHECK lt_doc_ref IS NOT INITIAL.

        lr_remessa = VALUE #( FOR <fs_doc_ref> IN lt_doc_ref USING KEY parent_btd WHERE
                            ( btd_tco = '73' )
                            ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                              option = /bobf/if_conf_c=>sc_sign_equal
                              low    = <fs_doc_ref>-btd_id+25(10) ) ).

        CHECK lr_remessa IS NOT INITIAL.

        SELECT vbeln,
               wbstk
          FROM likp
          INTO TABLE @DATA(lt_remessa)
         WHERE vbeln IN @lr_remessa.

        LOOP AT lt_remessa ASSIGNING FIELD-SYMBOL(<fs_remessa>) WHERE wbstk EQ 'C'.
          APPEND INITIAL LINE TO rt_messages ASSIGNING FIELD-SYMBOL(<fs_messages>).
          <fs_messages>-msgid       = gc_mgs-classe.
          <fs_messages>-msgno       = gc_mgs-num_016.
          <fs_messages>-parameter_1 = <fs_remessa>-vbeln.
          <fs_messages>-severity    = if_xo_const_message=>error.
        ENDLOOP.

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD check_selection_main.

    " Tabelas internas
    DATA: lt_compartimentos_keys TYPE /bobf/t_frw_key,
          lt_fu_root             TYPE /scmtms/t_tor_root_k,
          lt_tor_item            TYPE /scmtms/t_tor_item_tr_k,
          lt_fu_item             TYPE /scmtms/t_tor_item_tr_k.

    " Variáveis
    DATA: lv_compartimentos      TYPE char50.

    ev_result = gc_event_result-ok.

    lt_compartimentos_keys = it_compartimentos_keys.
    lt_fu_root             = it_fu_root.

    mo_tor_srvmgr->retrieve(
      EXPORTING
        iv_node_key = /scmtms/if_tor_c=>sc_node-item_tr                         " Node
        it_key      = lt_compartimentos_keys                                    " Key Table
      IMPORTING
        et_data     = lt_tor_item ).

    IF lt_compartimentos_keys IS INITIAL.
      " Compartimento não selecionado
      APPEND INITIAL LINE TO et_messages ASSIGNING FIELD-SYMBOL(<fs_msg>).
      <fs_msg>-msgid    = gc_mgs-classe.
      <fs_msg>-msgno    = gc_mgs-num_004.
      <fs_msg>-severity = if_xo_const_message=>error.
      ev_result         = gc_event_result-failed.
      RETURN.
    ENDIF.

    IF ms_selection-freight_unit-root IS INITIAL.
      " Selecionar uma unidade de frete
      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
      <fs_msg>-msgid    = gc_mgs-classe.
      <fs_msg>-msgno    = gc_mgs-num_005.
      <fs_msg>-severity = if_xo_const_message=>error.
      ev_result         = gc_event_result-failed.
      RETURN.
    ELSEIF lines( ms_selection-freight_unit-root ) > 1.
      " Selecionar apenas uma unidade de Frete
      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
      <fs_msg>-msgid    = gc_mgs-classe.
      <fs_msg>-msgno    = gc_mgs-num_006.
      <fs_msg>-severity = if_xo_const_message=>error.
      ev_result         = gc_event_result-failed.
      RETURN.
    ENDIF.

*    mo_tor_srvmgr->retrieve(
*      EXPORTING
*        iv_node_key = /scmtms/if_tor_c=>sc_node-item_tr                         " Node
*        it_key      = lt_compartimentos_keys                                    " Key Table
*      IMPORTING
*        et_data     = lt_tor_item ).
*
*    CHECK lt_tor_item IS NOT INITIAL.
*
*    mo_tor_srvmgr->retrieve(
*      EXPORTING
*        iv_node_key = /scmtms/if_tor_c=>sc_node-root                            " Node
*        it_key      = ms_selection-freight_unit-root                            " Key Table
*      IMPORTING
*        et_data     = lt_fu_root ).
*
*    CHECK lt_fu_root IS NOT INITIAL.
**********************************************************************
* BEGIN OF INSERT - 15.02.2024 - RDSP - GAPTM15
    DATA(lt_messages) = check_status_delivery( it_fu_root = lt_fu_root ). "#EC CI_CONV_OK

    IF lt_messages IS NOT INITIAL.
      et_messages = VALUE #( FOR <fs_messages> IN lt_messages
                           ( msgid        = <fs_messages>-msgid
                             msgno        = <fs_messages>-msgno
                             severity     = <fs_messages>-severity
                             parameter_1  = <fs_messages>-parameter_1 ) ).
      ev_result   = gc_event_result-failed.
      RETURN.
    ENDIF.
* END OF INSERT - 15.02.2024 - RDSP - GAPTM15
**********************************************************************
    mo_tor_srvmgr->retrieve_by_association(
      EXPORTING
        iv_node_key    = /scmtms/if_tor_c=>sc_node-root                         " Node
        it_key         = ms_selection-freight_unit-root                         " Key Table
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr          " Association
        iv_fill_data   = abap_true
      IMPORTING
        et_data        = lt_fu_item ).

    CHECK lt_fu_item IS NOT INITIAL.

    IF line_exists( lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ). "#EC CI_SORTSEQ
      DATA(ls_fu_item)  = lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]. "#EC CI_SORTSEQ
      DATA(ls_tor_item)  = VALUE #( lt_tor_item[ 1 ] OPTIONAL ).
    ELSE.
      RETURN.
    ENDIF.

    " Soma as capacidades dos compartimentos
    DATA(lv_cap_compart) = REDUCE /scmtms/qua_gro_vol_val( INIT lv_sun TYPE /scmtms/qua_gro_vol_val
                                                           FOR <fs_sun> IN lt_tor_item
                                                           NEXT lv_sun = lv_sun + <fs_sun>-gro_vol_valcap ).

    " Converte qtde da FU para a unidade de medida do compartimento
    DATA(lv_qty_fu) = convert_material_unit( is_item    = ls_tor_item           " Transportation Order Item
                                             is_fu_item = ls_fu_item ).         " Transportation Order FU Item

    " Concatena as descrições dos compartimentos
    CLEAR lv_compartimentos.
    LOOP AT lt_tor_item ASSIGNING FIELD-SYMBOL(<fs_compart>).
      IF lv_compartimentos IS INITIAL.
        lv_compartimentos = <fs_compart>-item_descr.
      ELSE.
        lv_compartimentos = lv_compartimentos && space && <fs_compart>-item_descr.
      ENDIF.
    ENDLOOP.

    " Qtde Fu menor que capacidade do compartimento
*    IF lv_qty_fu < ls_tor_item-gro_vol_valcap.
    IF lv_qty_fu < lv_cap_compart.

      " Quantidade da Unidade de Frete &1 é menor que compartimentos &2
      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
      <fs_msg>-msgid       = gc_mgs-classe.
      <fs_msg>-msgno       = gc_mgs-num_007.
      <fs_msg>-parameter_1 = |{ lt_fu_root[ 1 ]-tor_id ALPHA = OUT }|.
      <fs_msg>-parameter_2 = lv_compartimentos.
      <fs_msg>-severity    = if_xo_const_message=>error.
      ev_result            = gc_event_result-failed.
      RETURN.

      " Qtde Fu maior que capacidade do compartimento
*    ELSEIF lv_qty_fu > ls_tor_item-gro_vol_valcap.
    ELSEIF lv_qty_fu > lv_cap_compart.

      " Quantidade da Unidade de Frete &1 é maior que compartimentos &2
      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
      <fs_msg>-msgid       = gc_mgs-classe.
      <fs_msg>-msgno       = gc_mgs-num_008.
      <fs_msg>-parameter_1 = |{ lt_fu_root[ 1 ]-tor_id ALPHA = OUT }|.
      <fs_msg>-parameter_2 = lv_compartimentos.
      <fs_msg>-severity    = if_xo_const_message=>error.
      ev_result            = gc_event_result-failed.
      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD check_selection_special.

    " Tabelas internas
    DATA: lt_fu_item      TYPE /scmtms/t_tor_item_tr_k,
          lt_fu_root      TYPE /scmtms/t_tor_root_k,
          lt_tor_item     TYPE /scmtms/t_tor_item_tr_k,
          lt_tor_item_prd TYPE /scmtms/t_tor_item_tr_k.

    " Estrutura
    DATA: ls_fu_item_aux  TYPE /scmtms/s_tor_item_tr_k.

    " Variáveis
    DATA: lv_fu_id          TYPE char50,
          lv_compartimentos TYPE char50.

    lt_fu_root = it_fu_root.

    IF it_compartimentos_keys IS INITIAL.
      " Compartimento não selecionado
      APPEND INITIAL LINE TO et_messages ASSIGNING FIELD-SYMBOL(<fs_msg>).
      <fs_msg>-msgid    = gc_mgs-classe.
      <fs_msg>-msgno    = gc_mgs-num_004.
      <fs_msg>-severity = if_xo_const_message=>error.
      ev_result         = gc_event_result-failed.
      RETURN.
    ENDIF.

    mo_tor_srvmgr->retrieve( EXPORTING iv_node_key = /scmtms/if_tor_c=>sc_node-item_tr                                                    " Node
                                       it_key      = it_compartimentos_keys                                                               " Key Table
                             IMPORTING et_data     = lt_tor_item ).

    mo_tor_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root                                     " Node
                                                      it_key         = CORRESPONDING #( lt_tor_item MAPPING key = root_key )              " Key Table
                                                      iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr                      " Association
                                                      iv_fill_data   = abap_true                                                          " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
                                            IMPORTING et_data        = lt_tor_item_prd ).

    /scmtms/cl_tor_helper_common=>get_tor_data( EXPORTING it_root_key  = CORRESPONDING #( it_fu_root MAPPING key = key )                  " Key Table
                                                IMPORTING et_all_items = lt_fu_item ).                                                    " All items

    CHECK lt_fu_item IS NOT INITIAL.

    DATA(lt_product_id) = VALUE md_t_matnr( FOR <fs_emp> IN lt_fu_item  WHERE
                                          ( item_cat = /scmtms/if_tor_const=>sc_itemcat_prd )
                                          ( <fs_emp>-product_id ) ).

    DELETE ADJACENT DUPLICATES FROM lt_product_id COMPARING ALL FIELDS.

    IF lines( lt_product_id ) > 1.

      " Não é possível aloc. manual de produtos diferentes no mesmo compartimento
      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
      <fs_msg>-msgid    = gc_mgs-classe.
      <fs_msg>-msgno    = gc_mgs-num_017.
      <fs_msg>-severity = if_xo_const_message=>error.
      ev_result         = gc_event_result-failed.
      RETURN.

    ELSEIF lt_product_id IS INITIAL.

      " Selecionar uma unidade de frete
      APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
      <fs_msg>-msgid    = gc_mgs-classe.
      <fs_msg>-msgno    = gc_mgs-num_005.
      <fs_msg>-severity = if_xo_const_message=>error.
      ev_result         = gc_event_result-failed.
      RETURN.

    ELSEIF lines( lt_product_id ) = 1.
      IF lines( lt_fu_root ) = 1 .

        DATA(lt_messages) = check_status_delivery( it_fu_root = lt_fu_root ). "#EC CI_CONV_OK

        IF lt_messages IS NOT INITIAL.
          et_messages = VALUE #( FOR <fs_messages> IN lt_messages
                               ( msgid        = <fs_messages>-msgid
                                 msgno        = <fs_messages>-msgno
                                 severity     = <fs_messages>-severity
                                 parameter_1  = <fs_messages>-parameter_1 ) ).
          ev_result   = gc_event_result-failed.
          RETURN.
        ENDIF.

        mo_tor_srvmgr->retrieve_by_association(
          EXPORTING
            iv_node_key    = /scmtms/if_tor_c=>sc_node-root                         " Node
            it_key         = ms_selection-freight_unit-root                         " Key Table
            iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr          " Association
            iv_fill_data   = abap_true
          IMPORTING
            et_data        = lt_fu_item ).

        CHECK lt_fu_item IS NOT INITIAL.

        IF line_exists( lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ). "#EC CI_SORTSEQ
          DATA(ls_fu_item)  = lt_fu_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]. "#EC CI_SORTSEQ
          DATA(ls_tor_item)  = lt_tor_item[ 1 ].
        ELSE.
          RETURN.
        ENDIF.

        " Soma as capacidades dos compartimentos
        DATA(lv_cap_compart) = REDUCE /scmtms/qua_gro_vol_val( INIT lv_sun TYPE /scmtms/qua_gro_vol_val
                                                               FOR <fs_sun> IN lt_tor_item
                                                               NEXT lv_sun = lv_sun + <fs_sun>-gro_vol_valcap ).

        " Lista de compartimentos
        LOOP AT lt_tor_item ASSIGNING FIELD-SYMBOL(<fs_tor_item>).

          IF lt_tor_item_prd IS NOT INITIAL AND
             line_exists( lt_tor_item_prd[ item_parent_key = <fs_tor_item>-key
                                           item_cat        = /scmtms/if_tor_const=>sc_itemcat_prd ] ) AND
             NOT line_exists( lt_tor_item_prd[ item_parent_key = <fs_tor_item>-key
                                               item_cat        = /scmtms/if_tor_const=>sc_itemcat_prd
                                               product_id      = ls_fu_item-product_id ] ).


            " Selecionar uma unidade de frete
            APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
            <fs_msg>-msgid    = gc_mgs-classe.
            <fs_msg>-msgno    = gc_mgs-num_019.
            <fs_msg>-severity = if_xo_const_message=>error.
            ev_result         = gc_event_result-failed.
            DATA(lv_erro_dif_product_id) = abap_true.
            EXIT.

          ENDIF.

          " Soma de Unidades de Frete já associadas ao compartimento
          DATA(lv_qty_fu_aloc) = REDUCE /scmtms/qua_gro_vol_val( INIT lv_sun TYPE /scmtms/qua_gro_vol_val
                                                                 FOR <fs_sun> IN lt_tor_item_prd WHERE
                                                               ( item_parent_key = <fs_tor_item>-key AND
                                                                 item_cat = /scmtms/if_tor_const=>sc_itemcat_prd )
                                                                 NEXT lv_sun = lv_sun + <fs_sun>-gro_vol_val ).

          IF lv_qty_fu_aloc IS NOT INITIAL.
            ls_fu_item_aux-gro_vol_val = lv_qty_fu_aloc.
            ls_fu_item_aux-product_id  = ls_fu_item-product_id.
            ls_fu_item_aux-gro_vol_uni = ls_fu_item-gro_vol_uni.

            " Converte qtde da FU para a unidade de medida do compartimento
            DATA(lv_qty_fu) = convert_material_unit( is_item    = ls_tor_item           " Transportation Order Item
                                                     is_fu_item = ls_fu_item ).         " Transportation Order FU Item

            IF lv_qty_fu IS NOT INITIAL.
              lv_cap_compart = lv_cap_compart - lv_qty_fu.
            ENDIF.
            CLEAR: lv_qty_fu_aloc,
                   lv_qty_fu.
          ENDIF.
        ENDLOOP.

        CHECK lv_erro_dif_product_id = abap_false.

        " Converte qtde da FU para a unidade de medida do compartimento
        lv_qty_fu = convert_material_unit( is_item    = ls_tor_item           " Transportation Order Item
                                           is_fu_item = ls_fu_item ).         " Transportation Order FU Item

        " Concatena as descrições dos compartimentos
        CLEAR lv_compartimentos.
        LOOP AT lt_tor_item ASSIGNING FIELD-SYMBOL(<fs_compart>).
          IF lv_compartimentos IS INITIAL.
            lv_compartimentos = <fs_compart>-item_descr.
          ELSE.
            lv_compartimentos = lv_compartimentos && space && <fs_compart>-item_descr.
          ENDIF.
        ENDLOOP.

        " Qtde Fu maior que capacidade do compartimento
        IF lv_qty_fu > lv_cap_compart.

          " Quantidade da Unidade de Frete &1 é maior que compartimentos &2
          APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
          <fs_msg>-msgid       = gc_mgs-classe.
          <fs_msg>-msgno       = gc_mgs-num_008.
          <fs_msg>-parameter_1 = |{ lt_fu_root[ 1 ]-tor_id ALPHA = OUT }|.
          <fs_msg>-parameter_2 = lv_compartimentos.
          <fs_msg>-severity    = if_xo_const_message=>error.
          ev_result            = gc_event_result-failed.
          RETURN.

        ENDIF.

      ELSEIF lines( lt_fu_root ) > 1.
        IF lines( lt_tor_item ) > 1.

          " Selecionar apenas um compartimento.
          APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
          <fs_msg>-msgid    = gc_mgs-classe.
          <fs_msg>-msgno    = gc_mgs-num_018.
          <fs_msg>-severity = if_xo_const_message=>error.
          ev_result         = gc_event_result-failed.
          RETURN.

        ELSE.
          ls_tor_item = VALUE #( lt_tor_item[ 1 ] OPTIONAL ).
        ENDIF.

        ls_fu_item = VALUE #( lt_fu_item[ 1 ] OPTIONAL ).

        " Soma de Unidades de Frete já associadas ao compartimento
        lv_qty_fu_aloc = REDUCE /scmtms/qua_gro_vol_val( INIT lv_sun TYPE /scmtms/qua_gro_vol_val
                                                         FOR <fs_sun> IN lt_tor_item_prd WHERE
                                                       ( item_parent_key = ls_tor_item-key AND
                                                         item_cat = /scmtms/if_tor_const=>sc_itemcat_prd )
                                                         NEXT lv_sun = lv_sun + <fs_sun>-gro_vol_val ).

        " Soma as quantidades das FUs
        ls_fu_item-gro_vol_val = REDUCE /scmtms/qua_gro_vol_val( INIT lv_sun TYPE /scmtms/qua_gro_vol_val
                                                                 FOR <fs_sun> IN lt_fu_item WHERE
                                                               ( item_cat = /scmtms/if_tor_const=>sc_itemcat_prd )
                                                                 NEXT lv_sun = lv_sun + <fs_sun>-gro_vol_val ).

        " Converte qtde da FU para a unidade de medida do compartimento
        lv_qty_fu = convert_material_unit( is_item    = ls_tor_item           " Transportation Order Item
                                           is_fu_item = ls_fu_item ).         " Transportation Order FU Item

        " Concatena as descrições dos compartimentos
        CLEAR lv_fu_id.
        LOOP AT lt_fu_root ASSIGNING FIELD-SYMBOL(<fs_fu_id>).
          IF lv_fu_id IS INITIAL.
            lv_fu_id = |{ <fs_fu_id>-tor_id ALPHA = OUT }|.
          ELSE.
            lv_fu_id = lv_fu_id && '/' && |{ <fs_fu_id>-tor_id ALPHA = OUT }|.
          ENDIF.
        ENDLOOP.

        IF lv_qty_fu_aloc IS NOT INITIAL.
          lv_qty_fu = lv_qty_fu + lv_qty_fu_aloc.
        ENDIF.

        IF lv_qty_fu > ls_tor_item-gro_vol_valcap.
          " Quantidade da Unidade de Frete &1 é maior que compartimentos &2
          APPEND INITIAL LINE TO et_messages ASSIGNING <fs_msg>.
          <fs_msg>-msgid       = gc_mgs-classe.
          <fs_msg>-msgno       = gc_mgs-num_008.
          <fs_msg>-parameter_1 = lv_fu_id.
          <fs_msg>-parameter_2 = ls_tor_item-item_descr.
          <fs_msg>-severity    = if_xo_const_message=>error.
          ev_result            = gc_event_result-failed.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
