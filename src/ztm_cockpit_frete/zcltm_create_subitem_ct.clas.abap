CLASS zcltm_create_subitem_ct DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_t_item_tr TYPE TABLE OF /scmtms/s_tor_item_tr_k.

    CONSTANTS gc_item_type TYPE /scmtms/tor_item_type VALUE 'TRL' ##NO_TEXT.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLTM_CREATE_SUBITEM_CT IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    DATA: lt_item_tr      TYPE /scmtms/t_tor_item_tr_k,
          lt_item_tr_vres TYPE ty_t_item_tr,
          lt_mod          TYPE /bobf/t_frw_modification,
          lt_mod_temp     TYPE /bobf/t_frw_modification.

    DATA: ls_tor_item_info  TYPE /scmtms/s_tor_item_tr.

    io_read->retrieve( EXPORTING iv_node       = is_ctx-node_key
                                 it_key        = it_key
                       IMPORTING eo_message    = eo_message
                                 et_data       = lt_item_tr
                                 et_failed_key = et_failed_key ).

    LOOP AT lt_item_tr ASSIGNING FIELD-SYMBOL(<fs_item_tr>). "#EC CI_SORTSEQ

      CHECK <fs_item_tr>-item_type EQ gc_item_type.

      CLEAR: ls_tor_item_info.
      MOVE-CORRESPONDING <fs_item_tr> TO ls_tor_item_info.

      /scmtms/cl_tor_factory=>create_tor_item_vres( EXPORTING iv_item_key        = <fs_item_tr>-key                               " NodeID
                                                              iv_item_parent_key = <fs_item_tr>-item_parent_key                   " NodeID
                                                              iv_root_key        = <fs_item_tr>-root_key                          " NodeID
                                                              is_tor_item_info   = ls_tor_item_info                               " Structure for transient TOR item
                                                    IMPORTING et_tor_item        = DATA(lt_d_item_sub) ).                         " Main Area: Item

      IF lt_d_item_sub IS NOT INITIAL.
*
*        LOOP AT lt_d_item_sub ASSIGNING FIELD-SYMBOL(<fs_item_sub>) USING KEY root_itmcat
*           WHERE root_key EQ <fs_item_tr>-root_key
*             AND item_cat EQ /scmtms/if_tor_const=>sc_tor_item_category-ct_item. "#EC CI_NESTED
*
*          TRY.
*
*              io_modify->create( EXPORTING iv_node            = /scmtms/if_tor_c=>sc_node-item_tr                                 " Node to Create
*                                           iv_key             = <fs_item_sub>-key                                                 " NodeID
*                                           is_data            = REF #( <fs_item_sub> )                                            " Data
*                                           iv_assoc_key       = /scmtms/if_tor_c=>sc_association-root-item_tr                     " Association
*                                           iv_source_node_key = /scmtms/if_tor_c=>sc_node-root                                    " Parent Node
*                                           iv_source_key      = <fs_item_sub>-parent_key                                          " NodeID of Parent Instance
*                                           iv_root_key        = <fs_item_sub>-root_key
*                                 IMPORTING ev_key             = DATA(lt_key_item) ).
*            CATCH /bobf/cx_frw_contrct_violation.                                                                                 " Caller violates a BOPF contract
*              RETURN.
*          ENDTRY.
*        ENDLOOP.
*      ENDIF.
*    ENDLOOP.

        DELETE lt_d_item_sub WHERE item_cat NE /scmtms/if_tor_const=>sc_tor_item_category-ct_item.

        DATA(lv_item_id) = CONV numc10( 1000000 ).

        LOOP AT lt_d_item_sub REFERENCE INTO DATA(ls_item_sub).
          ls_item_sub->item_id                  = lv_item_id = lv_item_id + 1.
          ls_item_sub->src_stop_key             = <fs_item_tr>-src_stop_key.
          ls_item_sub->des_stop_key             = <fs_item_tr>-des_stop_key.
          ls_item_sub->gro_wei_val              = ls_item_sub->gro_vol_valcap.
          ls_item_sub->gro_wei_uni              = <fs_item_tr>-gro_wei_uni.
          ls_item_sub->gro_vol_val              = ls_item_sub->gro_vol_valcap.
          ls_item_sub->gro_vol_uni              = ls_item_sub->gro_vol_unicap.
          ls_item_sub->net_wei_val              = ls_item_sub->gro_vol_valcap.
          ls_item_sub->net_wei_uni              = <fs_item_tr>-gro_wei_uni.
          ls_item_sub->qua_pcs_uni              = <fs_item_tr>-qua_pcs_uni.
          ls_item_sub->pkgun_wei_uni            = <fs_item_tr>-gro_wei_uni.
          ls_item_sub->amt_gdsv_val             = <fs_item_tr>-amt_gdsv_val.
          ls_item_sub->amt_gdsv_cur             = <fs_item_tr>-amt_gdsv_cur.
          ls_item_sub->mot                      = <fs_item_tr>-mot.
          ls_item_sub->mot_cat                  = <fs_item_tr>-mot_cat.
          ls_item_sub->handling_exec            = <fs_item_tr>-handling_exec.
          ls_item_sub->load_plan_status         = <fs_item_tr>-load_plan_status.
          ls_item_sub->unload_plan_status       = <fs_item_tr>-unload_plan_status.
          ls_item_sub->load_plan_item_status    = <fs_item_tr>-load_plan_item_status.
          ls_item_sub->unload_plan_item_status  = <fs_item_tr>-unload_plan_item_status.
          ls_item_sub->orig_ref_item            = <fs_item_tr>-orig_ref_item.
          ls_item_sub->orig_ref_root            = <fs_item_tr>-orig_ref_root.
          ls_item_sub->orig_ref_bo              = <fs_item_tr>-orig_ref_bo.
          ls_item_sub->res_id                   = <fs_item_tr>-res_id.
        ENDLOOP.

        /scmtms/cl_mod_helper=>mod_create_multi( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-item_tr             " Node
                                                           it_data        = lt_d_item_sub
                                                           iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr " Association
                                                           iv_source_node = /scmtms/if_tor_c=>sc_node-root                " Node
                                                 CHANGING  ct_mod         = lt_mod ).                                     " Changes

      ENDIF.
    ENDLOOP.

    CHECK lt_mod IS NOT INITIAL.

    TRY.

        io_modify->do_modify( it_modification = lt_mod ).
        io_modify->end_modify( ).

      CATCH /bobf/cx_frw_contrct_violation.                                                                               " Caller violates a BOPF contract
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
