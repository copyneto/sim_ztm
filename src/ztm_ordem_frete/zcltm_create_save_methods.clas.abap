class ZCLTM_CREATE_SAVE_METHODS definition
  public
  final
  create public .

public section.

  methods DET_TSP_BY_VEHICLE
    importing
      !IO_METHPAR type ref to /SCTM/CL_METH_PARAMETER optional
      !IT_REQUEST type /SCTM/TT_REQUEST .
protected section.
private section.
ENDCLASS.



CLASS ZCLTM_CREATE_SAVE_METHODS IMPLEMENTATION.


  METHOD det_tsp_by_vehicle.

    DATA: lo_request           TYPE REF TO /sctm/cl_request,
          lo_tor_save_request  TYPE REF TO /scmtms/cl_chaco_request,
          lt_bokey_cond_result TYPE /scmtms/t_boid_cond_result,
          lo_srvmgr_tor        TYPE REF TO /bobf/if_tra_service_manager,
          lo_srvmgr_res        TYPE REF TO /bobf/if_tra_service_manager,
          lt_d_item_main       TYPE /scmtms/t_tor_item_tr_k,
          lo_message           TYPE REF TO /bobf/if_frw_message,
          ls_key               TYPE /bobf/s_frw_key,
          lt_key               TYPE /bobf/t_frw_key,
          lt_res_key           TYPE /bobf/t_frw_key,
          lt_link              TYPE /bobf/t_frw_key_link,
          lt_item_key          TYPE /bobf/t_frw_key,
          lt_failed_key        TYPE /bobf/t_frw_key,
          lv_cond_id           TYPE /scmtms/condition_id,
          lr_param             TYPE REF TO /scmtms/s_tor_root_a_asgn_tsp,
          ls_param_tsp         TYPE /scmtms/s_tor_tsp_assignment,
          lt_k_tor_root        TYPE /bobf/t_frw_key,
          lt_d_tor_root        TYPE /scmtms/t_tor_root_k,
          lt_d_res_root        TYPE /scmtms/t_res_root_k,
          lt_d_res_root_apoio  TYPE /scmtms/t_res_root_k,
          lt_d_res_combination TYPE /scmtms/t_res_combination_k.


    FIELD-SYMBOLS: <fs_bokey_cond_result> TYPE /scmtms/s_boid_cond_result,
                   <fs_d_tor_root>        TYPE /scmtms/s_tor_root_k,
                   <fs_d_item_main>       TYPE /scmtms/s_tor_item_tr_k,
                   <fs_d_res_root>        TYPE /scmtms/s_res_root_k.


    lo_srvmgr_tor = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_tor_c=>sc_bo_key ).

    IF io_methpar IS BOUND.
      LOOP AT it_request INTO lo_request.          "#EC CI_LOOP_INTO_WA
        lo_tor_save_request ?= lo_request.
        INSERT LINES OF lo_tor_save_request->mt_tor_key INTO TABLE lt_key.
      ENDLOOP.

      lo_srvmgr_tor->retrieve(
        EXPORTING
          iv_node_key = /scmtms/if_tor_c=>sc_node-root
          it_key      = lt_key
        IMPORTING
          et_data     = lt_d_tor_root
      ).

      "teste início

      " Busca a chave do veículo atribuido no nó item_tr_truck
      lo_srvmgr_tor->retrieve_by_association(
      EXPORTING
        iv_node_key = /scmtms/if_tor_c=>sc_node-root
        it_key = lt_key
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr
        iv_fill_data = abap_true
        iv_edit_mode = /bobf/if_conf_c=>sc_edit_read_only
      IMPORTING
        eo_message = lo_message
        et_data = lt_d_item_main
        et_key_link = lt_link
        et_target_key = lt_item_key
        et_failed_key = lt_failed_key ).

      "teste fim

      " Busca a chave do veículo atribuido no nó item_tr_truck
*      lo_srvmgr_tor->retrieve_by_association(
*      EXPORTING
*        iv_node_key = /scmtms/if_tor_c=>sc_node-root
*        it_key = lt_key
*        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr_truck
*        iv_fill_data = abap_true
*        iv_edit_mode = /bobf/if_conf_c=>sc_edit_read_only
*      IMPORTING
*        eo_message = lo_message
*        et_data = lt_d_item_main
*        et_key_link = lt_link
*        et_target_key = lt_item_key
*        et_failed_key = lt_failed_key ).

      LOOP AT lt_d_item_main ASSIGNING <fs_d_item_main> WHERE item_cat = 'AVR' OR item_cat = 'PVR'.
        REFRESH: lt_d_res_root_apoio, lt_res_key.

        IF  ( <fs_d_item_main>-item_cat = 'AVR' AND <fs_d_item_main>-mtr = 'ZMTR-TRK' ) OR
            ( <fs_d_item_main>-item_cat = 'PVR' AND <fs_d_item_main>-mtr = 'ZMTR-CARR' ).

          APPEND INITIAL LINE TO lt_res_key ASSIGNING FIELD-SYMBOL(<fs_res_key>).
          <fs_res_key>-key = <fs_d_item_main>-res_key.
        ENDIF.

*       BUSCA O RECURSO
        IF lt_res_key[] IS NOT INITIAL.

          lo_srvmgr_res = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_resource_c=>sc_bo_key ).

          IF io_methpar IS BOUND.
            lo_srvmgr_res->retrieve(
              EXPORTING
                iv_node_key = /scmtms/if_resource_c=>sc_node-root
                it_key      = lt_res_key
              IMPORTING
                "et_data     = lt_d_res_root ).
                et_data     = lt_d_res_root_apoio ).
          ENDIF.
        ENDIF.

        IF lt_d_res_root_apoio[] IS NOT INITIAL.
          INSERT LINES OF lt_d_res_root_apoio INTO TABLE lt_d_res_root[].
        ENDIF.

      ENDLOOP.

      LOOP AT lt_d_res_root ASSIGNING <fs_d_res_root> WHERE owneruid IS NOT INITIAL.
        ls_param_tsp-tsp_key = <fs_d_res_root>-owneruid.
      ENDLOOP.

      CREATE DATA lr_param.

      LOOP AT lt_d_tor_root ASSIGNING <fs_d_tor_root>.
        ls_param_tsp-tor_key = <fs_d_tor_root>-key.

        INSERT ls_param_tsp INTO TABLE lr_param->tsp_assignment.
        ls_key-key = <fs_d_tor_root>-key.
        INSERT ls_key INTO TABLE lt_k_tor_root.

      ENDLOOP.

    ENDIF.

    lo_srvmgr_tor->do_action(
      EXPORTING
        iv_act_key    = /scmtms/if_tor_c=>sc_action-root-assign_tsp
        it_key        = lt_k_tor_root
        is_parameters = lr_param ).

  ENDMETHOD.
ENDCLASS.
