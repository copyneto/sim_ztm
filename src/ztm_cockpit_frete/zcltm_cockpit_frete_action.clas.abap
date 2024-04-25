***********************************************************************
***                             © REDE SIM                          ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Cockpit de Transporte - Botões - Área Caminhões        *
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

CLASS zcltm_cockpit_frete_action DEFINITION
  PUBLIC
  INHERITING FROM /scmtms/cl_ui_tcact_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /scmtms/if_ui_tcact~get_action_ids REDEFINITION.

  PROTECTED SECTION.

    METHODS prepare REDEFINITION.

    METHODS check_selection REDEFINITION.

    METHODS execute REDEFINITION.

    METHODS get_selection REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCLTM_COCKPIT_FRETE_ACTION IMPLEMENTATION.


  METHOD /scmtms/if_ui_tcact~get_action_ids.

    " Informa os botões que iremos manipular
    rt_action_ids = VALUE #( ( zcltm_cockpit_frete_event=>gc_action-new_tor )
                             ( zcltm_cockpit_frete_event=>gc_action-tor_transf )
                             ( zcltm_cockpit_frete_event=>gc_action-saida_mercadoria ) ).

  ENDMETHOD.


  METHOD check_selection.

    DATA(lo_event) = zcltm_cockpit_frete_event=>get_instance( ).

    lo_event->check_selection_new_tor_fob( EXPORTING io_event     = io_event
                                                     is_selection = me->ms_selection
                                           IMPORTING ev_result    = ev_result
                                                     et_messages  = et_messages ).

    lo_event->check_selection_saida_merc( EXPORTING io_event     = io_event
                                                    is_selection = me->ms_selection
                                          IMPORTING ev_result    = ev_result
                                                    et_messages  = et_messages ).
  ENDMETHOD.


  METHOD get_selection.

    super->get_selection( io_event           = io_event
                          iv_clear_selection = abap_false ).

  ENDMETHOD.


  METHOD prepare.

    DATA(lo_event) = zcltm_cockpit_frete_event=>get_instance( ).

    lo_event->prepare_new_tor_fob( EXPORTING io_controller          = io_controller
                                             io_log                 = io_log
                                             io_mp_tc               = io_mp_tc
                                             io_tbo_srvmgr          = io_tbo_srvmgr
                                             io_tor_srvmgr          = io_tor_srvmgr
                                             io_event               = io_event
                                             iv_block_mw_processing = iv_block_mw_processing
                                   IMPORTING ev_result              = ev_result
                                             et_messages            = et_messages ).

    lo_event->prepare_saida_merc( EXPORTING io_controller          = io_controller
                                            io_log                 = io_log
                                            io_mp_tc               = io_mp_tc
                                            io_tbo_srvmgr          = io_tbo_srvmgr
                                            io_tor_srvmgr          = io_tor_srvmgr
                                            io_event               = io_event
                                            iv_block_mw_processing = iv_block_mw_processing
                                  IMPORTING ev_result              = ev_result
                                            et_messages            = et_messages ).

  ENDMETHOD.


  METHOD execute.

    DATA(lo_event) = zcltm_cockpit_frete_event=>get_instance( ).

    lo_event->execute_new_tor_fob( EXPORTING io_event                   = io_event
                                             is_selection               = me->ms_selection
                                   IMPORTING ev_suppress_dirty_handling = ev_suppress_dirty_handling
                                             ev_trigger_save            = ev_trigger_save
                                             ev_trigger_refresh         = ev_trigger_refresh
                                             ev_trigger_deselect        = ev_trigger_deselect
                                             ev_result                  = ev_result
                                             et_messages                = et_messages ).

    lo_event->execute_saida_merc( EXPORTING io_event                   = io_event
                                            is_selection               = me->ms_selection
                                  IMPORTING ev_suppress_dirty_handling = ev_suppress_dirty_handling
                                            ev_trigger_save            = ev_trigger_save
                                            ev_trigger_refresh         = ev_trigger_refresh
                                            ev_trigger_deselect        = ev_trigger_deselect
                                            ev_result                  = ev_result
                                            et_messages                = et_messages ).

  ENDMETHOD.

ENDCLASS.
