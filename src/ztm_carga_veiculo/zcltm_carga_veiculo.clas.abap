class ZCLTM_CARGA_VEICULO definition
  public
  final
  create public .

public section.

  methods MAIN
    importing
      !IT_VEHICLE_RESOURCES type /SCMTMS/T_SB_VEH_RES_COMBI_DEF .
  methods RETURN_MESSAGE
    importing
      !P_TASK type ANY .
  class-methods SAVE_LOG
    importing
      !IT_MESSAGE type ZCTGTM_LOG_RES_COM .
protected section.
private section.

  data:
    gt_messages TYPE TABLE OF zttm_log_res_com .
  constants:
    BEGIN OF gc_message,
      id         TYPE symsgid VALUE 'ZTM_CARGA_VEICULO',
      number_000 TYPE symsgno VALUE '000',
      number_001 TYPE symsgno VALUE '001',
      number_002 TYPE symsgno VALUE '002',
      number_003 TYPE symsgno VALUE '003',
      number_004 TYPE symsgno VALUE '004',
    END OF gc_message .
  constants:
    BEGIN OF gc_equitype,
      con TYPE /scmb/de_equi_type VALUE 'CON',
      cav TYPE /scmb/de_equi_type VALUE 'CAV',
      car TYPE /scmb/de_equi_type VALUE 'CAR',
    END OF gc_equitype .
  data GV_MESSAGE type FLAG .

  methods GET_VEHICLE_RESOURCES_KEYS
    importing
      !IT_VEHICLE_RESOURCES type /SCMTMS/T_SB_VEH_RES_COMBI_DEF
    exporting
      !ET_VRES_ID_KEY type /SCMTMS/IF_SB_ESET_RESOUR_DAC=>TY_T_VRES_ID_KEY
      !ET_RES_ROOT type /SCMTMS/T_RES_ROOT_K .
  methods PREPARE_VEHICLE_RESOURCES
    importing
      !IT_VEHICLE_RESOURCES type /SCMTMS/T_SB_VEH_RES_COMBI_DEF
      !IT_RES_ROOT type /SCMTMS/T_RES_ROOT_K
      !IT_VRES_ID_KEY type /SCMTMS/IF_SB_ESET_RESOUR_DAC=>TY_T_VRES_ID_KEY
    exporting
      !ET_COMBI_RES_DEF type /SCMB/CL_TMSRES_HELPER=>TYT_COMBINATION_DEF .
  methods CREATE_RESOURCE_COMBI
    importing
      !IT_COMBI_RES_DEF type /SCMB/CL_TMSRES_HELPER=>TYT_COMBINATION_DEF .
  methods ADD_MESSAGE
    importing
      !IV_COMBINATION_RESOURCE_ID type /SCMTMS/RES_NAME_VEH
      !IV_RESOURCE_ID type /SCMTMS/RES_NAME_VEH optional
      !IV_MSGTY type SYMSGTY
      !IV_MSGID type SYMSGID
      !IV_MSGNO type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional .
ENDCLASS.



CLASS ZCLTM_CARGA_VEICULO IMPLEMENTATION.


  METHOD add_message.

    APPEND INITIAL LINE TO gt_messages ASSIGNING FIELD-SYMBOL(<fs_msg>).
    <fs_msg>-mandt                    = sy-mandt.
    <fs_msg>-combination_resource_id  = iv_combination_resource_id.
    <fs_msg>-resource_id              = iv_resource_id.
    <fs_msg>-msgty                    = iv_msgty.
    <fs_msg>-msgid                    = iv_msgid.
    <fs_msg>-msgno                    = iv_msgno.
    <fs_msg>-message_v1               = iv_message_v1.
    <fs_msg>-message_v2               = iv_message_v2.
    <fs_msg>-message_v3               = iv_message_v3.
    <fs_msg>-message_v4               = iv_message_v4.
    <fs_msg>-created_by               = sy-uname.

    CONVERT DATE sy-datum TIME sy-uzeit
       INTO TIME STAMP <fs_msg>-created_at TIME ZONE sy-zonlo.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = iv_msgid
        msgnr               = iv_msgno
        msgv1               = iv_message_v1
        msgv2               = iv_message_v2
        msgv3               = iv_message_v3
        msgv4               = iv_message_v4
      IMPORTING
        message_text_output = <fs_msg>-message.

  ENDMETHOD.


  METHOD create_resource_combi.

    DATA: lt_combi_res_def TYPE zctgtm_combination_def,
          lt_message       TYPE zctgtm_log_res_com.

    CLEAR: lt_combi_res_def.
    lt_combi_res_def = CORRESPONDING #( it_combi_res_def ).
    lt_message       = CORRESPONDING #( gt_messages ).

    CHECK lt_combi_res_def IS NOT INITIAL.

    CALL FUNCTION 'ZFMTM_CREATE_RESOURCE_COMBI'
      STARTING NEW TASK 'CREATE_RESOURCE_COMBI'
      CALLING me->return_message ON END OF TASK
      EXPORTING
        it_combi_res_def = lt_combi_res_def
        it_message       = lt_message.

    WAIT UNTIL me->gv_message IS NOT INITIAL.

    RETURN.
  ENDMETHOD.


  METHOD get_vehicle_resources_keys.

    DATA: lt_veh_res_ids TYPE /scmtms/t_res_name,
          lt_res_root    TYPE /scmtms/t_res_root_k.

    DATA: ls_vres_id_key       TYPE /scmtms/if_sb_eset_resour_dac=>ty_s_vres_id_key.

    DATA(lo_vres_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_resource_c=>sc_bo_key ).

    LOOP AT it_vehicle_resources ASSIGNING FIELD-SYMBOL(<fs_vehicle_res>).

      IF NOT line_exists( lt_veh_res_ids[ table_line = <fs_vehicle_res>-combination_resource_id ] ). "#EC CI_STDSEQ
        APPEND <fs_vehicle_res>-combination_resource_id TO lt_veh_res_ids.
      ENDIF.

      IF NOT line_exists( lt_veh_res_ids[ table_line = <fs_vehicle_res>-resource_id ] ).             "#EC CI_STDSEQ
        APPEND <fs_vehicle_res>-resource_id             TO lt_veh_res_ids.
      ENDIF.

    ENDLOOP.

    TRY.

        DATA(lt_parameters) = VALUE /bobf/t_frw_query_selparam( FOR <fs_res_id> IN lt_veh_res_ids
                                                              ( attribute_name = /scmtms/if_resource_c=>sc_query_attribute-root-qu_by_attributes-res_id
                                                                sign           = /bobf/if_conf_c=>sc_sign_option_including
                                                                option         = /bobf/if_conf_c=>sc_sign_equal
                                                                low            = <fs_res_id> ) ).

        lo_vres_srv_mgr->query( EXPORTING iv_query_key            = /scmtms/if_resource_c=>sc_query-root-qu_by_attributes " Query
                                          it_selection_parameters = lt_parameters                                         " Query Selection Parameters
                                          iv_fill_data            = abap_true                                             " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
                                IMPORTING et_data                 = lt_res_root ).

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        RETURN.
    ENDTRY.

    SORT lt_res_root BY res_id.

    LOOP AT lt_veh_res_ids ASSIGNING FIELD-SYMBOL(<fs_veh_res_id>).

      READ TABLE lt_res_root ASSIGNING FIELD-SYMBOL(<fs_result>) WITH KEY res_id = <fs_veh_res_id> BINARY SEARCH.

      IF sy-subrc EQ 0.

        CLEAR ls_vres_id_key.
        ls_vres_id_key-vehicle_res_id = <fs_veh_res_id>.
        ls_vres_id_key-vehicle_res_key = <fs_result>-key.
        INSERT ls_vres_id_key INTO TABLE et_vres_id_key.

      ENDIF.

    ENDLOOP.

    et_res_root = lt_res_root.

  ENDMETHOD.


  METHOD main.

    DATA: lt_vehicle_resources TYPE /scmtms/t_sb_veh_res_combi_def.

    lt_vehicle_resources = it_vehicle_resources.

    " Busca Chave de veículos
    get_vehicle_resources_keys( EXPORTING it_vehicle_resources = lt_vehicle_resources                 " Vehicle Resource Combination Definition
                                IMPORTING et_vres_id_key       = DATA(lt_vres_id_key)
                                          et_res_root          = DATA(lt_res_root) ).                 " Root Node of the TMS Resource

    CHECK lt_vres_id_key IS NOT INITIAL.

    " Valida e prepara dados
    prepare_vehicle_resources( EXPORTING it_vehicle_resources = lt_vehicle_resources                  " Vehicle Resource Combination Definition
                                         it_res_root          = lt_res_root                           " Root Node of the TMS Resource
                                         it_vres_id_key       = lt_vres_id_key
                               IMPORTING et_combi_res_def     = DATA(lt_combi_res_def) ).

    " Cria recurso combinado
    create_resource_combi( it_combi_res_def = lt_combi_res_def ).

  ENDMETHOD.


  METHOD prepare_vehicle_resources.

    DATA: lt_veh_res_ids       TYPE /scmtms/t_res_name,
          lt_veh_res_ids_comb  TYPE /scmtms/t_res_name,
          lt_veh_res_ids_carr  TYPE /scmtms/t_res_name,
          lt_res_root          TYPE /scmtms/t_res_root_k,
          lt_combi_res_def     TYPE /scmb/cl_tmsres_helper=>tyt_combination_def,
          lt_vehicle_resources TYPE /scmtms/t_sb_veh_res_combi_def.

    DATA: lr_veh_res_ids_comb  TYPE RANGE OF /scmtms/res_name.

    DATA: ls_combi_res_def     TYPE /scmb/cl_tmsres_helper=>tys_combination_def.

    lt_vehicle_resources = it_vehicle_resources.

    " Separa os recursos combinados
    CLEAR: lt_veh_res_ids_comb.
    lt_veh_res_ids_comb = VALUE #( FOR <fs_comb> IN it_vehicle_resources
                                 ( <fs_comb>-combination_resource_id ) ).

    DELETE ADJACENT DUPLICATES FROM lt_veh_res_ids_comb COMPARING ALL FIELDS.

    " Separa os recursos
    CLEAR: lt_veh_res_ids_carr.
    lt_veh_res_ids_carr = VALUE #( FOR <fs_carr> IN it_vehicle_resources
                                 ( <fs_carr>-resource_id ) ).

    DELETE ADJACENT DUPLICATES FROM lt_veh_res_ids_carr COMPARING ALL FIELDS.

    SORT: lt_veh_res_ids,
          lt_veh_res_ids_comb,
          lt_veh_res_ids_carr.

    " Valida recursos combinados.
    LOOP AT lt_veh_res_ids_comb ASSIGNING FIELD-SYMBOL(<fs_veh_res_ids_comb>).

      " Valida se o recurso combinado existe no SAP.
      IF line_exists( it_res_root[ KEY res_id res_id = <fs_veh_res_ids_comb> ] ).

        " Verifica se o recuro combinado é do tipo CON
        IF it_res_root[ KEY res_id res_id = <fs_veh_res_ids_comb> ]-equitype NE gc_equitype-con.

          DELETE lt_vehicle_resources USING KEY combination_resource_id
            WHERE combination_resource_id EQ <fs_veh_res_ids_comb>. "#EC CI_STDSEQ

          " Não é possível montar combinação. Recurso &1 não é do tipo CON.
          add_message( iv_combination_resource_id = <fs_veh_res_ids_comb>                                                                 " Recurso
                       iv_msgty                   = if_xo_const_message=>error                                                            " Tipo de mensagem
                       iv_msgid                   = gc_message-id                                                                         " Classe de mensagem
                       iv_msgno                   = gc_message-number_001                                                                 " Nº mensagem
                       iv_message_v1              = CONV symsgv( <fs_veh_res_ids_comb> ) ).                                               " Variável mensagens

        ENDIF.
      ELSE.

        DELETE lt_vehicle_resources USING KEY combination_resource_id
          WHERE combination_resource_id EQ <fs_veh_res_ids_comb>. "#EC CI_STDSEQ

        " Não é possível montar combinação. Recurso &1 não foi criado no SAP.
        add_message( iv_combination_resource_id = <fs_veh_res_ids_comb>                                                                   " Recurso
                     iv_msgty                   = if_xo_const_message=>error                                                              " Tipo de mensagem
                     iv_msgid                   = gc_message-id                                                                           " Classe de mensagem
                     iv_msgno                   = gc_message-number_003                                                                   " Nº mensagem
                     iv_message_v1              = CONV symsgv( <fs_veh_res_ids_comb> ) ).                                                 " Variável mensagens

      ENDIF.
    ENDLOOP.

    " Valida recursos.
    LOOP AT lt_veh_res_ids_carr ASSIGNING FIELD-SYMBOL(<fs_veh_res_ids_carr>).

      " Valida se o recurso combinado existe no SAP.
      IF line_exists( it_res_root[ KEY res_id res_id = <fs_veh_res_ids_carr> ] ).

        " Verifica se o recuro combinado é do tipo CON
        IF it_res_root[ KEY res_id res_id = <fs_veh_res_ids_carr> ]-equitype NE gc_equitype-cav AND
           it_res_root[ KEY res_id res_id = <fs_veh_res_ids_carr> ]-equitype NE gc_equitype-car.

          CLEAR: lt_veh_res_ids_comb.
          lr_veh_res_ids_comb = VALUE #( FOR <fs_comb> IN it_vehicle_resources WHERE
                                       ( resource_id = <fs_veh_res_ids_carr> )
                                       ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                                         option = /bobf/if_conf_c=>sc_sign_equal
                                         low = <fs_comb>-combination_resource_id ) ). "#EC CI_STDSEQ

          DELETE ADJACENT DUPLICATES FROM lr_veh_res_ids_comb COMPARING low.
          DELETE lt_vehicle_resources WHERE combination_resource_id IN lr_veh_res_ids_comb. "#EC CI_STDSEQ

          LOOP AT lr_veh_res_ids_comb ASSIGNING FIELD-SYMBOL(<fs_rg_res_ids_comb>). "#EC CI_NESTED

            " Não é possível montar combinação. Recurso &1 é do tipo &2.
            add_message( iv_combination_resource_id = <fs_rg_res_ids_comb>-low                                                            " Recurso
                         iv_resource_id             = <fs_veh_res_ids_carr>                                                               " Recurso
                         iv_msgty                   = if_xo_const_message=>error                                                          " Tipo de mensagem
                         iv_msgid                   = gc_message-id                                                                       " Classe de mensagem
                         iv_msgno                   = gc_message-number_002                                                               " Nº mensagem
                         iv_message_v1              = CONV symsgv( <fs_veh_res_ids_comb> )                                                " Variável mensagens
                         iv_message_v2              = CONV symsgv( it_res_root[ KEY res_id res_id = <fs_veh_res_ids_carr> ]-equitype ) ). " Variável mensagens

          ENDLOOP.
        ENDIF.
      ELSE.

        CLEAR: lt_veh_res_ids_comb.
        lr_veh_res_ids_comb = VALUE #( FOR <fs_comb> IN it_vehicle_resources WHERE
                                     ( resource_id = <fs_veh_res_ids_carr> )
                                     ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                                       option = /bobf/if_conf_c=>sc_sign_equal
                                       low = <fs_comb>-combination_resource_id ) ). "#EC CI_STDSEQ

        DELETE ADJACENT DUPLICATES FROM lr_veh_res_ids_comb COMPARING low.
        DELETE lt_vehicle_resources WHERE combination_resource_id IN lr_veh_res_ids_comb. "#EC CI_STDSEQ

        LOOP AT lr_veh_res_ids_comb ASSIGNING <fs_rg_res_ids_comb>. "#EC CI_NESTED

          " Não é possível montar combinação. Recurso &1 é do tipo &2.
          add_message( iv_combination_resource_id = <fs_rg_res_ids_comb>-low                                                              " Recurso
                       iv_resource_id             = <fs_veh_res_ids_carr>                                                                 " Recurso
                       iv_msgty                   = if_xo_const_message=>error                                                            " Tipo de mensagem
                       iv_msgid                   = gc_message-id                                                                         " Classe de mensagem
                       iv_msgno                   = gc_message-number_003                                                                 " Nº mensagem
                       iv_message_v1              = CONV symsgv( <fs_veh_res_ids_carr> ) ).                                               " Variável mensagens

        ENDLOOP.

      ENDIF.
    ENDLOOP.

    CHECK lt_vehicle_resources IS NOT INITIAL.

    " Adicona mensagem de sucesso
    LOOP AT lt_vehicle_resources ASSIGNING FIELD-SYMBOL(<fs_vehicle_resources>).
      " Combinação &1 criada com sucesso.
      add_message( iv_combination_resource_id = <fs_vehicle_resources>-combination_resource_id                                            " Recurso
                   iv_resource_id             = <fs_vehicle_resources>-resource_id                                                        " Recurso
                   iv_msgty                   = if_xo_const_message=>success                                                              " Tipo de mensagem
                   iv_msgid                   = gc_message-id                                                                             " Classe de mensagem
                   iv_msgno                   = gc_message-number_004                                                                     " Nº mensagem
                   iv_message_v1              = CONV symsgv( <fs_vehicle_resources>-combination_resource_id ) ).                          " Variável mensagens
    ENDLOOP.

    LOOP AT lt_vehicle_resources ASSIGNING FIELD-SYMBOL(<fs_group_attr>)
      GROUP BY ( key1 = <fs_group_attr>-combination_resource_id )
      ASSIGNING FIELD-SYMBOL(<fs_veh_res_group>).

      " Check Combination Resource ID
      READ TABLE it_vres_id_key ASSIGNING FIELD-SYMBOL(<fs_combi_res_id_key>)
        WITH TABLE KEY vehicle_res_id = <fs_veh_res_group>-key1.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      LOOP AT GROUP <fs_veh_res_group> ASSIGNING FIELD-SYMBOL(<fs_veh_combi_def>).
        " Check Individual Resource ID
        IF  <fs_veh_combi_def>-resource_id IS NOT INITIAL.
          READ TABLE it_vres_id_key ASSIGNING FIELD-SYMBOL(<fs_indiv_res_id_key>)
            WITH TABLE KEY vehicle_res_id = <fs_veh_combi_def>-resource_id.
          IF sy-subrc EQ 0.
            ls_combi_res_def = VALUE #( tmsresuuid    = <fs_combi_res_id_key>-vehicle_res_key
                                        seq_num       = <fs_veh_combi_def>-sequence_no
                                        equi_type     = <fs_veh_combi_def>-equipment_group
                                        equi_code     = <fs_veh_combi_def>-equipment_type
                                        indiv_resuuid = <fs_indiv_res_id_key>-vehicle_res_key ).
          ENDIF.
        ELSE.
          ls_combi_res_def = VALUE #( tmsresuuid    = <fs_combi_res_id_key>-vehicle_res_key
                                      seq_num       = <fs_veh_combi_def>-sequence_no
                                      equi_type     = <fs_veh_combi_def>-equipment_group
                                      equi_code     = <fs_veh_combi_def>-equipment_type ).
        ENDIF.
        APPEND ls_combi_res_def TO et_combi_res_def.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD return_message.

    RECEIVE RESULTS FROM FUNCTION 'ZFMTM_SISPETRO_SEND_DATA'
      IMPORTING
        ev_return = me->gv_message.

    RETURN.
  ENDMETHOD.


  METHOD save_log.

    DATA: lt_message TYPE TABLE OF zttm_log_res_com.

    lt_message = CORRESPONDING #( it_message ).

    CHECK lt_message IS NOT INITIAL.

    MODIFY zttm_log_res_com FROM TABLE lt_message.

  ENDMETHOD.
ENDCLASS.
