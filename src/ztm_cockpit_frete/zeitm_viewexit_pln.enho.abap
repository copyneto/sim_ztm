CLASS lcl_zeitm_viewexit_pln DEFINITION DEFERRED.
CLASS /scmtms/cl_ui_viewexit_pln DEFINITION LOCAL FRIENDS lcl_zeitm_viewexit_pln.
CLASS lcl_zeitm_viewexit_pln DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zeitm_viewexit_pln.      "#EC NEEDED
    DATA core_object TYPE REF TO /scmtms/cl_ui_viewexit_pln . "#EC NEEDED
 INTERFACES  IPO_ZEITM_VIEWEXIT_PLN.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO /scmtms/cl_ui_viewexit_pln OPTIONAL.
ENDCLASS.
CLASS lcl_zeitm_viewexit_pln IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD ipo_zeitm_viewexit_pln~adapt_event.
*"------------------------------------------------------------------------*
*" Declaration of POST-method, do not insert any comments here please!
*"
*"methods ADAPT_EVENT
*"  importing
*"    !IV_BO_KEY type /BOBF/OBM_BO_KEY
*"    !IV_NODE_KEY type /BOBF/OBM_NODE_KEY
*"    !IV_EVENTID type STRING
*"    !IR_EVENT_LIST_LINE type ref to DATA optional
*"    !IR_EVENT_DATA type DATA optional
*"    !IT_SELECTED_ROWS type /BOFU/IF_FBI_VIEW_INSTANCE_NEW=>TT_ROW optional
*"    !IO_ADDITIONAL_INFO type ref to OBJECT optional
*"    !IV_RAISED_BY_OWN_UI type BOOLE_D optional
*"    !IS_UI_CONTEXT_DATA type /BOFU/IF_FBI_VIEW_INSTANCE_NEW=>TS_UI_CONTEXT_DATA optional
*"    !IO_UI_INFO type ref to IF_FPM_LIST_ATS_UI_INFO optional
*"  changing
*"    !CV_FAILED type BOOLE_D
*"    !CV_EVENT_RESULT_DEFER type BOOLE_D optional .
*"------------------------------------------------------------------------*

    TRY.
        DATA(lo_event) = zcltm_cockpit_frete_event=>get_instance( ).

        lo_event->exit_adapt_event( EXPORTING iv_bo_key             = iv_bo_key
                                              iv_node_key           = iv_node_key
                                              iv_eventid            = iv_eventid
                                              ir_event_list_line    = ir_event_list_line
                                              ir_event_data         = ir_event_data
                                              it_selected_rows      = it_selected_rows
                                              io_additional_info    = io_additional_info
                                              iv_raised_by_own_ui   = iv_raised_by_own_ui
                                              is_ui_context_data    = is_ui_context_data
                                              io_ui_info            = io_ui_info
                                    CHANGING  cv_failed             = cv_failed
                                              cv_event_result_defer = cv_event_result_defer ).

* ---------------------------------------------------------------------------
* Cockpit de Frete - Caminhões - Botão: Criar documento de Frete
* ---------------------------------------------------------------------------
        IF iv_eventid = zcltm_cockpit_frete_event=>gc_action-new_tor.

          me->core_object->_set_selected_objects( EXPORTING iv_list_object   = me->core_object->mv_list_object
                                                            iv_eventid       = CONV #( zcltm_cockpit_frete_event=>gc_action-old_tor )
                                                            it_selected_rows = it_selected_rows ).

          me->core_object->_set_selected_objects( EXPORTING iv_list_object   = me->core_object->mv_list_object
                                                            iv_eventid       = CONV #( zcltm_cockpit_frete_event=>gc_action-new_tor )
                                                            it_selected_rows = it_selected_rows ).
        ENDIF.

* ---------------------------------------------------------------------------
* Cockpit de Frete - Caminhões - Botão: Transferência
* ---------------------------------------------------------------------------
        IF iv_eventid = zcltm_cockpit_frete_event=>gc_action-tor_transf.

          me->core_object->_set_selected_objects( EXPORTING iv_list_object   = me->core_object->mv_list_object
                                                            iv_eventid       = CONV #( zcltm_cockpit_frete_event=>gc_action-old_tor )
                                                            it_selected_rows = it_selected_rows ).

          me->core_object->_set_selected_objects( EXPORTING iv_list_object   = me->core_object->mv_list_object
                                                            iv_eventid       = CONV #( zcltm_cockpit_frete_event=>gc_action-tor_transf )
                                                            it_selected_rows = it_selected_rows ).
        ENDIF.

* ---------------------------------------------------------------------------
* Cockpit de Frete - Ordem de Frete - Botão: Saída de Mercadoria
* ---------------------------------------------------------------------------
        IF iv_eventid = zcltm_cockpit_frete_event=>gc_action-saida_mercadoria.

          me->core_object->_set_selected_objects( EXPORTING iv_list_object   = me->core_object->mv_list_object
                                                            iv_eventid       = CONV #( zcltm_cockpit_frete_event=>gc_action-saida_mercadoria )
                                                            it_selected_rows = it_selected_rows ).
        ENDIF.

* ---------------------------------------------------------------------------
* Cockpit de Frete - Botão: Alocação Manual
* ---------------------------------------------------------------------------
        IF iv_eventid = 'ZTB_ALOC_MANUAL_ACTION'.

          me->core_object->_set_selected_objects( EXPORTING iv_list_object   = me->core_object->mv_list_object
                                                            iv_eventid       = 'ZTB_ALOC_MANUAL_ACTION'
                                                            it_selected_rows = it_selected_rows ).
        ENDIF.

      CATCH cx_root INTO DATA(lo_root).
        DATA(lv_msg) = CONV bapi_msg( lo_root->get_longtext( ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
