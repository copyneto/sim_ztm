FUNCTION zfmtm_aloc_manual_split_fu.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_MAIN) TYPE  ZCTGTM_ALOC_MANUAL
*"  EXPORTING
*"     VALUE(ET_MAIN) TYPE  ZCTGTM_ALOC_MANUAL
*"     VALUE(EV_RETURN) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA: lt_param   TYPE /scmtms/t_tor_root_a_doc_split,
        lt_fu_root TYPE /scmtms/t_tor_root_k,
        lt_fu_item TYPE /scmtms/t_tor_item_tr_k,
        lt_key     TYPE /bobf/t_frw_key.

  DATA: lv_fu_root TYPE /bobf/conf_key.

  DATA(lo_srvmgr_tor) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).

  LOOP AT it_main ASSIGNING FIELD-SYMBOL(<fs_main>).

    IF lv_fu_root IS NOT INITIAL.
      <fs_main>-unid_frete = lv_fu_root.
      CLEAR: lv_fu_root.
    ENDIF.

    CALL METHOD /scmtms/cl_tor_helper_common=>get_tor_data
      EXPORTING
        it_root_key  = VALUE #( ( key = <fs_main>-unid_frete ) )
      IMPORTING
        et_root      = lt_fu_root
        et_all_items = lt_fu_item.

    DELETE lt_fu_item WHERE item_cat <> /scmtms/if_tor_const=>sc_itemcat_prd. "#EC CI_SORTSEQ
    CHECK lt_fu_item IS NOT INITIAL.

    IF <fs_main>-tor_id IS INITIAL.
      <fs_main>-tor_id = lt_fu_root[ 1 ]-tor_id.
    ENDIF.

    IF <fs_main>-capac_split IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: lt_param,
           lt_key.

    APPEND INITIAL LINE TO lt_key ASSIGNING FIELD-SYMBOL(<fs_key>).
    <fs_key>-key = <fs_main>-unid_frete.

    APPEND INITIAL LINE TO lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).
    <fs_param>-keep_capa_assignment = /scmtms/if_tor_const=>sc_tor_split_capa_assgn-keep_capa_assgn.
    <fs_param>-split_mode           = /scmtms/if_tor_const=>sc_tor_split_mode-create_target_doc.
    <fs_param>-src_tor_root_key     = <fs_key>-key.
    <fs_param>-split_item_data      = VALUE #( ( src_item_key   = lt_fu_item[ 1 ]-key
                                                 split_qty_attr = /scmtms/if_tor_c=>sc_query_attribute-item_tr-qdb_query_by_attributes-gro_vol_val
                                                 split_qty      = <fs_main>-capac_split
                                                 split_qty_uom  = <fs_main>-capac_split_unid ) ).

    TRY.
        lo_srvmgr_tor->do_action(
          EXPORTING
            iv_act_key    = /scmtms/if_tor_c=>sc_action-root-split_fu               " Action
            it_key        = lt_key                                                  " Key Table
            is_parameters = REF #( lt_param )                                       " Action
          IMPORTING
            eo_message    = DATA(lo_message)
            eo_change     = DATA(lo_change) ).

        DATA(lt_change) = lo_change->get_changes( ).
        CHECK lt_change IS NOT INITIAL.
        DATA(lo_changes) = lt_change[ 1 ]-change_object.
        lo_changes->get_changes( EXPORTING iv_change_mode = /scmtms/if_tor_const=>sc_tor_split_mode-create_target_doc
                                           iv_node_key    = /scmtms/if_tor_c=>sc_node-root
                                 IMPORTING et_change      = DATA(lt_change_n) ).    " Node Table

        CHECK lt_change_n IS NOT INITIAL.
        lv_fu_root = lt_change_n[ 1 ]-key.

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        RETURN.
    ENDTRY.
  ENDLOOP.

  et_main[] = it_main[].
  ev_return = abap_true.

  DATA(lo_trans_manager) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
  lo_trans_manager->save( EXPORTING iv_transaction_pattern = /bobf/if_tra_c=>gc_tp_save_and_continue
                          IMPORTING eo_change              = lo_change
                                    eo_message             = lo_message ).

  lo_trans_manager->cleanup( EXPORTING iv_cleanup_mode = '2'                        " Transaction Behavior During Save (Transaction Mode)
                             RECEIVING eo_message      = lo_message ).              " Interface of Message Object


ENDFUNCTION.
