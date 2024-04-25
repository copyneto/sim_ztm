*&---------------------------------------------------------------------*
*& Report ZTMR_AJUSTE_DADOS_FSD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztmr_ajuste_dados_fsd.

DATA: gv_sfir_id         TYPE /scmtms/sfir_id,
      gv_sfir_type       TYPE /scmtms/sfir_type,
      gv_cr_by           TYPE /bofu/user_id_created_by,
      gv_cr_on           TYPE /bofu/tstmp_creation_time,
      gv_po              TYPE /scmb/pur_org_ext_id,
      gv_pg              TYPE /scmb/egrp_org_ext_id,
      gv_tsp             TYPE /scmtms/pty_trans_srv_provider,
      lt_fixed_val       TYPE ddfixvalues,
      gv_return          TYPE boolean VALUE abap_false,
      lo_transaction_mgr TYPE REF TO /bobf/if_tra_transaction_mgr.


DATA: lo_sfir_mgr           TYPE REF TO /bobf/if_tra_service_manager,
      lt_sfir_root_data_er  TYPE /scmtms/t_sfir_root_k,
      lt_sfir_root_data_tmp TYPE /scmtms/t_sfir_root_k,
      lt_locked_sfir        TYPE /scmtms/t_sfir_root_k,
      lv_lines              TYPE i,
      lr_sfir_data          TYPE REF TO /scmtms/s_sfir_root_k,
      ls_mod                TYPE /bobf/s_frw_modification,
      lt_changed_field      TYPE /bobf/t_frw_name,
      lt_mod                TYPE  /bobf/t_frw_modification,
      lo_message_1          TYPE REF TO /bobf/if_frw_message,
      lt_symsg              TYPE /scmtms/t_symsg,
      lt_sfir_root_data     TYPE /scmtms/t_sfir_root_k,
      lt_sfir_root_key      TYPE /bobf/t_frw_key,
      lt_failed_key         TYPE /bobf/t_frw_key,
      lt_parameters         TYPE        /bobf/t_frw_query_selparam,
      ls_parameter          TYPE /bobf/s_frw_query_selparam.

TYPES:
  BEGIN OF s_rfc_data,
    BEGIN OF s_importing, "For Parallel Processing
      sfir_root_keys TYPE /bobf/t_frw_key,
      prog           TYPE sycprog,
      title          TYPE sytitle,
    END OF s_importing,

    BEGIN OF s_exporting,
      t_bal_msg TYPE /scmtms/t_bal_s_msg,
    END OF s_exporting,
  END OF s_rfc_data.

FIELD-SYMBOLS:
             <ls_fixed_val>          LIKE LINE OF lt_fixed_val.

*&---------------------------------------------------------------------*
*&  Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-000.
* Selection settings
  SELECTION-SCREEN SKIP 1.

  SELECT-OPTIONS: so_sf_id FOR gv_sfir_id, "SFIR ID
                  so_sf_ty FOR gv_sfir_type,  "SFIR Type
                  so_cr_on FOR gv_cr_on NO-DISPLAY,    " Creation Date
                  so_po    FOR gv_po,   " Purchase Org
                  so_pg    FOR gv_pg MATCHCODE OBJECT /scmb/sh_orgunit_purgrp,   " Purchase Group
                  so_cr_by FOR gv_cr_by. "Created By

  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS: so_tsp FOR gv_tsp MATCHCODE OBJECT bupa.   " Party ID

  SELECTION-SCREEN SKIP 1.

  PARAMETERS: pv_test TYPE /scmtms/test_mode DEFAULT abap_true.

SELECTION-SCREEN END OF BLOCK sel.


INITIALIZATION.

* Set transaction manager to dialog mode
  CALL METHOD /scmtms/cl_sfir_posting_helper=>init_dialog_transaction_mgr
    CHANGING
      cv_transaction_mgr = lo_transaction_mgr.   " Interface of the standalone transaction manager
  /scmtms/cl_sfir_helper_root=>set_batch_mode( ).

  lo_sfir_mgr ?= /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_suppfreightinvreq_c=>sc_bo_key ).

  PERFORM build_sel_para CHANGING lt_parameters.

  IF gv_return EQ abap_true.
    RETURN.
  ENDIF.

  lo_sfir_mgr->query(
    EXPORTING
      iv_query_key            = /scmtms/if_suppfreightinvreq_c=>sc_query-root-query_by_elements
      it_selection_parameters = lt_parameters
    IMPORTING
      et_key                  = lt_sfir_root_key ).

  lo_sfir_mgr->retrieve(
  EXPORTING
      it_key = lt_sfir_root_key
      iv_node_key = /scmtms/if_suppfreightinvreq_c=>sc_node-root
      iv_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive
  IMPORTING
      et_data = lt_sfir_root_data
      et_failed_key = lt_failed_key
  ).

  IF lt_failed_key IS NOT INITIAL.
    lo_sfir_mgr->retrieve(
    EXPORTING
        it_key = lt_failed_key
        iv_node_key = /scmtms/if_suppfreightinvreq_c=>sc_node-root
    IMPORTING
        et_data = lt_locked_sfir
    ).
    LOOP AT lt_locked_sfir ASSIGNING FIELD-SYMBOL(<ls_failed_sfir>).
      WRITE: / 'Following documents locked in different session:',
             / <ls_failed_sfir>-sfir_id.
    ENDLOOP.
  ENDIF.

  IF lt_sfir_root_data IS INITIAL.
    INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 172 msgv1 = lv_lines ) INTO TABLE lt_symsg.
  ENDIF.

  ls_mod-node                    = /scmtms/if_suppfreightinvreq_c=>sc_node-root.
  ls_mod-change_mode             = /bobf/if_frw_c=>sc_modify_update.
  INSERT /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-lifecycle INTO TABLE ls_mod-changed_fields .
  INSERT /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation INTO TABLE ls_mod-changed_fields .


  lt_sfir_root_data_er = lt_sfir_root_data.
*  Message for PO Creation failed
  DELETE lt_sfir_root_data_er WHERE lifecycle NE /scmtms/if_sfir_status=>sc_root-lifecycle-v_transferred_for_accruals.
  CLEAR: lv_lines.
  lv_lines = lines( lt_sfir_root_data_er ).
  IF lv_lines IS NOT INITIAL.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_no_confirmation_from_erp.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 167 msgv1 = lv_lines ) INTO TABLE lt_symsg. "po creation failed
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_ready_for_accruals
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
* remove conf call
    ENDIF.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_ses_abd_posting_initiated.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 168 msgv1 = lv_lines ) INTO TABLE lt_symsg. "ses creation failed
* remove lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_ses_abd_creation_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_abd_cr_started.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 192 msgv1 = lv_lines ) INTO TABLE lt_symsg. "abd creation failed
* remove lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_abd_creation_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
  ENDIF.

  lt_sfir_root_data_er = lt_sfir_root_data.
*  Message for Cleanup failed
  DELETE lt_sfir_root_data_er WHERE lifecycle NE /scmtms/if_sfir_status=>sc_root-lifecycle-v_po_cancel_initiated.
  CLEAR: lv_lines.
  lv_lines = lines( lt_sfir_root_data_er ).
  IF lv_lines IS NOT INITIAL.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_no_confirmation_from_erp.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 170 msgv1 = lv_lines ) INTO TABLE lt_symsg. "ses_cancel_failed
* remove lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_ses_abd_cancel_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_po_cancel_initiated.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 169 msgv1 = lv_lines ) INTO TABLE lt_symsg. "po cancellation failed
* remove lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_po_cancel_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
  ENDIF.

  lt_sfir_root_data_er = lt_sfir_root_data.
*  Message for Cancellation failed
  DELETE lt_sfir_root_data_er WHERE lifecycle NE /scmtms/if_sfir_status=>sc_root-lifecycle-v_cancellation_requested_in_er.
  CLEAR: lv_lines.
  lv_lines = lines( lt_sfir_root_data_er ).
  IF lv_lines IS NOT INITIAL.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_no_confirmation_from_erp.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 170 msgv1 = lv_lines ) INTO TABLE lt_symsg. "ses_cancel_failed
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_accruals_posted
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_confirmed_from_erp
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_po_cancel_initiated.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 169 msgv1 = lv_lines ) INTO TABLE lt_symsg. "po cancellation failed
* remove lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_po_cancel_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_abd_cancellation_started.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 193 msgv1 = lv_lines ) INTO TABLE lt_symsg. "po cancellation failed
* remove lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_ab_cancel_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
  ENDIF.

  lt_sfir_root_data_er = lt_sfir_root_data.
*  Message for Reversal failed
  DELETE lt_sfir_root_data_er WHERE lifecycle NE /scmtms/if_sfir_status=>sc_root-lifecycle-v_reversal_requested_in_erp.
  CLEAR: lv_lines.
  lv_lines = lines( lt_sfir_root_data_er ).
  IF lv_lines IS NOT INITIAL.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_no_confirmation_from_erp.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 171 msgv1 = lv_lines ) INTO TABLE lt_symsg. "ses_cancel_failed
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_invoice_verified
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_confirmed_from_erp
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
    CLEAR: lv_lines.
    lt_sfir_root_data_tmp = lt_sfir_root_data_er.
    DELETE lt_sfir_root_data_tmp WHERE confirmation NE /scmtms/if_sfir_status=>sc_root-confirmation-v_abd_reverse_started.
    lv_lines = lines( lt_sfir_root_data_tmp ).
    IF lv_lines IS NOT INITIAL.
      INSERT VALUE #( msgty = 'I' msgid = '/SCMTMS/SFIR' msgno = 194 msgv1 = lv_lines ) INTO TABLE lt_symsg. "po cancellation failed
* removed lc call
      lo_sfir_mgr->do_action(
      EXPORTING
      iv_act_key = /scmtms/if_suppfreightinvreq_c=>sc_action-root-set_ab_reverse_failed
      it_key = CORRESPONDING /bobf/t_frw_key( lt_sfir_root_data_tmp MAPPING key = key )
      IMPORTING
      et_failed_key = lt_failed_key
      ).
    ENDIF.
  ENDIF.

* Before processing the sfirs
  /scmtms/cl_sfir_batch_helper=>collect_symsg_to_messageobj(
    EXPORTING
      it_symsg             =  lt_symsg   " System Messages
      iv_bo_key            =   /scmtms/if_suppfreightinvreq_c=>sc_bo_key  " Business Object
      iv_node              =   /scmtms/if_suppfreightinvreq_c=>sc_node-root  " Node Name
      iv_detlevel          =   /scmtms/cl_applog_helper=>sc_al_s_level-level_2 " Application Log: Level of detail
      iv_probclass         =   /scmtms/cl_applog_helper=>sc_al_probclass_very_important    " Application log: Message problem class
      iv_add_context_info  =   abap_true   " Generate BO instance specific message context
    CHANGING
      co_message           =  lo_message_1  " Interface of Message Object
  ).

  /scmtms/cl_sfir_batch_helper=>write_formatted_msg_to_output( "add the message no relevant for summary in this method.
    EXPORTING
      io_message  =  lo_message_1   " Interface of Message Object
      iv_heading  =  'Messages'
  ).

  CLEAR lo_message_1.

  IF pv_test IS INITIAL.
    /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->save( IMPORTING eo_message = lo_message_1 ).
  ENDIF.

  WRITE: 'done'.

END-OF-SELECTION.

FORM build_sel_para  CHANGING ct_parameters TYPE /bobf/t_frw_query_selparam.

  DATA: lt_org_r          TYPE /scmb/t_slsorg_range,
        ls_org_r          TYPE /scmb/s_slsorg_range,
        lt_org_map        TYPE /scmb/t_orgunit,
        lt_org_map_1      TYPE /scmb/t_orgunit,
        lt_bfp_parameters TYPE TABLE OF /bobf/s_frw_query_selparam, "n2266052
        lo_srv_mgr        TYPE REF TO /bobf/if_tra_service_manager,
        lt_bfp_data       TYPE /bofu/t_bupa_root_k,
        ls_bfp_data       TYPE /bofu/s_bupa_root_k.

  FIELD-SYMBOLS:
        <ls_org_map> LIKE LINE OF lt_org_map.

  CLEAR ls_parameter.
  IF NOT so_sf_id IS INITIAL.
    ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-sfir_id.
    LOOP AT so_sf_id.
      MOVE-CORRESPONDING so_sf_id TO ls_parameter.
      APPEND ls_parameter TO lt_parameters.
    ENDLOOP.
  ENDIF.

  IF NOT so_sf_ty IS INITIAL.
    ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-sfir_type.
    LOOP AT so_sf_ty.
      MOVE-CORRESPONDING so_sf_ty TO ls_parameter.
      APPEND ls_parameter TO ct_parameters.
    ENDLOOP.
  ENDIF.

* Fill the selection parameters SFIR creation date:
  IF NOT so_cr_on IS INITIAL.
    ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-created_on.
    LOOP AT so_cr_on.
      MOVE-CORRESPONDING so_cr_on TO ls_parameter.
      APPEND ls_parameter TO ct_parameters.
    ENDLOOP.
  ENDIF.

* Fill the selection parameters Purch Org:
  IF NOT so_po IS INITIAL.
    CLEAR lt_org_r.
    LOOP AT so_po.
      MOVE-CORRESPONDING so_po TO ls_org_r.
      APPEND ls_org_r TO lt_org_r.
    ENDLOOP.
    /scmtms/cl_direct_db_access=>get_org_by_ext_range(
      EXPORTING
        iv_org_role     = /scmb/if_org_unit_c=>c_orgunit_role-organisation
        iv_org_function = /scmb/if_org_unit_c=>c_orgunit_function-purchasing
        it_org_extid_r  = lt_org_r
      IMPORTING
        et_org_id_map   = lt_org_map_1 ).
    APPEND LINES OF lt_org_map_1 TO lt_org_map.
    CLEAR lt_org_map_1.
    /scmtms/cl_direct_db_access=>get_org_by_ext_range(
      EXPORTING
        iv_org_role     = /scmb/if_org_unit_c=>c_orgunit_role-organisation
        iv_org_function = /scmb/if_org_unit_c=>c_orgunit_function-forwarding_house
        it_org_extid_r  = lt_org_r
      IMPORTING
        et_org_id_map   = lt_org_map_1 ).
    APPEND LINES OF lt_org_map_1 TO lt_org_map.
    CLEAR lt_org_map_1.
    IF lt_org_map IS INITIAL.
      gv_return = abap_true.
      RETURN.
    ENDIF.
    ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-purch_org_id.
    ls_parameter-option         = 'EQ'.                     "#EC NOTEXT
    ls_parameter-sign           = 'I'.                      "#EC NOTEXT
    CLEAR ls_parameter-high.
    LOOP AT lt_org_map ASSIGNING <ls_org_map>.
      ls_parameter-low = <ls_org_map>-id.
      APPEND ls_parameter TO ct_parameters.
    ENDLOOP.
  ENDIF.

* Fill the selection parameters Purchase Group:
  IF NOT so_pg IS INITIAL.
    CLEAR lt_org_r.
    LOOP AT so_pg.
      MOVE-CORRESPONDING so_pg TO ls_org_r.
      APPEND ls_org_r TO lt_org_r.
    ENDLOOP.
    /scmtms/cl_direct_db_access=>get_org_by_ext_range(
      EXPORTING
        iv_org_role     = /scmb/if_org_unit_c=>c_orgunit_role-group
        iv_org_function = /scmb/if_org_unit_c=>c_orgunit_function-purchasing
        it_org_extid_r  = lt_org_r
      IMPORTING
        et_org_id_map   = lt_org_map_1 ).
    APPEND LINES OF lt_org_map_1 TO lt_org_map.
    CLEAR lt_org_map_1.
    /scmtms/cl_direct_db_access=>get_org_by_ext_range(
      EXPORTING
        iv_org_role     = /scmb/if_org_unit_c=>c_orgunit_role-group
        iv_org_function = /scmb/if_org_unit_c=>c_orgunit_function-forwarding_house
        it_org_extid_r  = lt_org_r
      IMPORTING
        et_org_id_map   = lt_org_map_1 ).
    APPEND LINES OF lt_org_map_1 TO lt_org_map.
    CLEAR lt_org_map_1.
    IF lt_org_map IS INITIAL.
      gv_return = abap_true.
      RETURN.
    ENDIF.
    ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-purch_grp_id.
    ls_parameter-option         = 'EQ'.                     "#EC NOTEXT
    ls_parameter-sign           = 'I'.                      "#EC NOTEXT
    CLEAR ls_parameter-high.
    LOOP AT lt_org_map ASSIGNING <ls_org_map>.
      ls_parameter-low = <ls_org_map>-id.
      APPEND ls_parameter TO ct_parameters.
    ENDLOOP.
  ENDIF.

*   Created By
  IF so_cr_by IS NOT INITIAL.
    LOOP AT so_cr_by ASSIGNING FIELD-SYMBOL(<ls_created_by>).
      CLEAR ls_parameter.
      ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-created_by.
      ls_parameter-sign           = <ls_created_by>-sign.
      ls_parameter-option         = <ls_created_by>-option.
      ls_parameter-low            = <ls_created_by>-low.
      ls_parameter-high           = <ls_created_by>-high.
      APPEND ls_parameter TO ct_parameters.
    ENDLOOP.
  ENDIF.

  IF NOT so_tsp IS INITIAL.
    LOOP AT so_tsp.
      IF NOT so_tsp-high IS INITIAL.
        ls_parameter-option         = 'BT'.
      ELSE.
        ls_parameter-option          = 'EQ'.
      ENDIF.
      ls_parameter-attribute_name = /bofu/if_bupa_constants=>sc_query_attribute-root-qu_by_names_and_key_wrds-partner.
      ls_parameter-sign           = 'I'.
      ls_parameter-low            = so_tsp-low.
      ls_parameter-high           = so_tsp-high.

      INSERT ls_parameter INTO TABLE lt_bfp_parameters.
    ENDLOOP.
  ENDIF.

  lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /bofu/if_bupa_constants=>sc_bo_key ).

  IF lt_bfp_parameters IS NOT INITIAL.
    lo_srv_mgr->query(
      EXPORTING
        iv_query_key             = /bofu/if_bupa_constants=>sc_query-root-qu_by_names_and_key_wrds
        it_selection_parameters  = lt_bfp_parameters
        iv_fill_data             = abap_true
      IMPORTING
        et_data                  = lt_bfp_data
    ).
  ENDIF.

  CLEAR ls_parameter.
  LOOP AT lt_bfp_data INTO ls_bfp_data.
    ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-bill_from_party.
    ls_parameter-sign           = 'I'.
    IF so_tsp-option = 'BT'.
      ls_parameter-option = 'EQ'.
    ELSEIF so_tsp-option = 'NB'.
      ls_parameter-option = 'NE'.
    ELSE.
      ls_parameter-option = so_tsp-option.
    ENDIF.

    ls_parameter-low            = ls_bfp_data-key.
    APPEND ls_parameter TO ct_parameters.
  ENDLOOP.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "life cycle statuses
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-lifecycle.
  ls_parameter-option         = 'EQ'.
  ls_parameter-sign           = 'I'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-lifecycle-v_transferred_for_accruals.
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-lifecycle.
  ls_parameter-option         = 'EQ'.
  ls_parameter-sign           = 'I'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-lifecycle-v_po_cancel_initiated.
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-lifecycle.
  ls_parameter-option         = 'EQ'.
  ls_parameter-sign           = 'I'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-lifecycle-v_cancellation_requested_in_er.
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-lifecycle.
  ls_parameter-option         = 'EQ'.
  ls_parameter-sign           = 'I'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-lifecycle-v_reversal_requested_in_erp.
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-lifecycle.
  ls_parameter-option         = 'EQ'.
  ls_parameter-sign           = 'I'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-lifecycle-v_fci_transferred_for_posting.
  APPEND ls_parameter TO ct_parameters.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "confirmation statuses
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation.
  ls_parameter-sign           = 'I'.
  ls_parameter-option         = 'EQ'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-confirmation-v_no_confirmation_from_erp. "include only not posted & documents for which PO was cancelled
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation.
  ls_parameter-sign           = 'I'.
  ls_parameter-option         = 'EQ'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-confirmation-v_ses_abd_posting_initiated. "include only not posted & documents for which PO was cancelled
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation.
  ls_parameter-sign           = 'I'.
  ls_parameter-option         = 'EQ'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-confirmation-v_abd_cr_started. "include only not posted & documents for which PO was cancelled
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation.
  ls_parameter-sign           = 'I'.
  ls_parameter-option         = 'EQ'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-confirmation-v_po_cancel_initiated. "include only not posted & documents for which PO was cancelled
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation.
  ls_parameter-sign           = 'I'.
  ls_parameter-option         = 'EQ'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-confirmation-v_abd_cancellation_started. "include only not posted & documents for which PO was cancelled
  APPEND ls_parameter TO ct_parameters.

  CLEAR ls_parameter.
  ls_parameter-attribute_name = /scmtms/if_suppfreightinvreq_c=>sc_node_attribute-root-confirmation.
  ls_parameter-sign           = 'I'.
  ls_parameter-option         = 'EQ'.
  ls_parameter-low            = /scmtms/if_sfir_status=>sc_root-confirmation-v_abd_reverse_started. "include only not posted & documents for which PO was cancelled
  APPEND ls_parameter TO ct_parameters.
ENDFORM.
