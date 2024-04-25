CLASS zcltm_cadastrar_motorista DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_behv
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_reported TYPE RESPONSE FOR REPORTED EARLY zi_tm_motoristas,

      ty_r_types  TYPE RANGE OF bu_id_type,

      BEGIN OF ty_parameter,
        retail_rfc_destination TYPE string,
        partner_group          TYPE bapibus1006_head-partn_grp,
        types                  TYPE ty_r_types,
      END OF ty_parameter.

    CONSTANTS:

      BEGIN OF gc_param_destination,
        modulo TYPE ztca_param_mod-modulo VALUE 'BP'                ##NO_TEXT,
        chave1 TYPE ztca_param_par-chave1 VALUE 'CROSS_CLIENT'      ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'DESTINATION_RFC'   ##NO_TEXT,
        chave3 TYPE ztca_param_par-chave3 VALUE 'OIL_TO_RETAIL'     ##NO_TEXT,
      END OF gc_param_destination,

      BEGIN OF gc_param_moto_agrupamento,
        modulo TYPE ztca_param_mod-modulo VALUE 'TM'                ##NO_TEXT,
        chave1 TYPE ztca_param_par-chave1 VALUE 'MOTORISTAS'        ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'AGRUPAMENTO'       ##NO_TEXT,
      END OF gc_param_moto_agrupamento,

      BEGIN OF gc_param_moto_tipo,
        modulo TYPE ztca_param_mod-modulo VALUE 'TM'                ##NO_TEXT,
        chave1 TYPE ztca_param_par-chave1 VALUE 'MOTORISTAS'        ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'DOCUMENTOS'        ##NO_TEXT,
      END OF gc_param_moto_tipo,

      BEGIN OF gc_message,
        id     TYPE symsgid     VALUE 'ZTM_MOTORISTAS',
        no_002 TYPE symsgno     VALUE '002',
        no_005 TYPE symsgno     VALUE '005',
        no_006 TYPE symsgno     VALUE '006',
        no_007 TYPE symsgno     VALUE '007',
        no_008 TYPE symsgno     VALUE '008',
        no_009 TYPE symsgno     VALUE '009',
        no_010 TYPE symsgno     VALUE '010',
        no_011 TYPE symsgno     VALUE '011',
        no_012 TYPE symsgno     VALUE '012',
        no_013 TYPE symsgno     VALUE '013',
        no_014 TYPE symsgno     VALUE '014',
        no_015 TYPE symsgno     VALUE '015',
        no_016 TYPE symsgno     VALUE '016',
        no_017 TYPE symsgno     VALUE '017',
        no_018 TYPE symsgno     VALUE '018',
        no_019 TYPE symsgno     VALUE '019',
        no_020 TYPE symsgno     VALUE '020',
      END OF gc_message,

      BEGIN OF gc_cds,
        motoristas TYPE string VALUE 'ZI_TM_MOTORISTAS'          ##NO_TEXT,
        doc_add    TYPE string VALUE 'ZI_TM_DOC_ADD'             ##NO_TEXT,
      END OF gc_cds,

      BEGIN OF gc_action,
        insert TYPE c VALUE 'I',
        update TYPE c VALUE 'U',
        delete TYPE c VALUE 'D',
      END OF gc_action,

      BEGIN OF gc_reported,
        msg     TYPE string VALUE '%msg',
        element TYPE string VALUE '%element',
      END OF gc_reported,

      BEGIN OF gc_field,
        businesspartnerredesim TYPE string VALUE 'BUSINESSPARTNERREDESIM',
        cpf                    TYPE string VALUE 'CPF',
        Nome                   TYPE string VALUE 'NOME',
        cnh                    TYPE string VALUE 'CNH',
        Validadecnh            TYPE string VALUE 'VALIDADECNH',
        Categoriacnh           TYPE string VALUE 'CATEGORIACNH',
        tipo                   TYPE string VALUE 'TIPO',
        numero                 TYPE string VALUE 'NUMERO',
        validadedesde          TYPE string VALUE 'VALIDADEDESDE',
        validade               TYPE string VALUE 'VALIDADE',
      END OF gc_field.

    METHODS:constructor
      IMPORTING
        is_motora TYPE zi_tm_motoristas OPTIONAL
        it_docs   TYPE zctgtm_doc_add OPTIONAL.

    METHODS execute
      EXPORTING es_motora        TYPE zi_tm_motoristas
                et_docs          TYPE zctgtm_doc_add
      RETURNING VALUE(rt_return) TYPE bapiret2_t.

    CLASS-METHODS save
      IMPORTING is_motora  TYPE zi_tm_motoristas OPTIONAL
                it_doc_add TYPE zctgtm_doc_add OPTIONAL
      EXPORTING et_return  TYPE bapiret2_t.

    CLASS-METHODS task_finish
      IMPORTING
        p_task TYPE clike.

    METHODS get_parameter
      EXPORTING es_parameter TYPE ty_parameter
                et_return    TYPE bapiret2_t.

    METHODS valida_motorista
      IMPORTING iv_operation TYPE cfopera DEFAULT 'I'
                is_motorista TYPE zi_tm_motoristas
      EXPORTING et_return    TYPE bapiret2_t.

    "! Constrói mensagens retorno do aplicativo
    METHODS build_reported
      IMPORTING
        !it_return   TYPE bapiret2_t
      EXPORTING
        !es_reported TYPE ty_reported.

    METHODS check_valid_doc_types
      IMPORTING ir_types TYPE ty_r_types
      CHANGING  ct_docs  TYPE zctgtm_doc_add.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      gs_motora    TYPE zi_tm_motoristas,
      gt_docs      TYPE zctgtm_doc_add,
      gv_result    TYPE char1,
      gt_return    TYPE bapiret2_t,
      gs_parameter TYPE ty_parameter.

    METHODS call_bapis
      IMPORTING is_centraldataperson TYPE bapibus1006_central_person
      RETURNING VALUE(rt_return)     TYPE bapiret2_t.

    METHODS fill_centraldataperson
      RETURNING
        VALUE(rs_result) TYPE bapibus1006_central_person.

    METHODS call_bupa_create_from_data
      IMPORTING
        is_centraldataperson TYPE bapibus1006_central_person
      RETURNING
        VALUE(rt_return)     TYPE bapiret2_t.

    METHODS call_bupa_tax_add
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.

    METHODS call_bupa_role_add_2
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.

    METHODS call_identification_add
      IMPORTING iv_action        TYPE char01
      RETURNING VALUE(rt_return) TYPE bapiret2_t.

    METHODS call_replication_retail_to_oil
      IMPORTING
        iv_action        TYPE char01
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.

ENDCLASS.



CLASS zcltm_cadastrar_motorista IMPLEMENTATION.


  METHOD execute.

    FREE: es_motora, et_docs.

    DATA(ls_centraldataperson) = fill_centraldataperson(  ).

    rt_return = call_bapis( is_centraldataperson = ls_centraldataperson ).
    es_motora = gs_motora.
    et_docs   = gt_docs.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    gt_docs   = it_docs.
    gs_motora = is_motora.

  ENDMETHOD.


  METHOD call_bapis.

    DATA(lv_action) = COND #( WHEN gs_motora-bp IS INITIAL
                              THEN gc_action-insert
                              ELSE gc_action-update ).

    DATA(lt_return) = call_bupa_create_from_data( is_centraldataperson ).

    " Parceiro &1 encontrado com o CPF informado. Preparando para atualizar.
    DATA(lv_action_replicate) = COND #( WHEN line_exists( lt_return[ id         = gc_message-id
                                                                     number     = gc_message-no_007 ] )
                                        THEN gc_action-update
                                        ELSE lv_action ).

    INSERT LINES OF lt_return INTO TABLE rt_return.
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>error ] ).
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>warning ] ).

    lt_return = call_identification_add( iv_action = lv_action  ).

    INSERT LINES OF lt_return INTO TABLE rt_return.
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>error ] ).
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>warning ] ).

    lt_return = call_bupa_tax_add(  ).

    INSERT LINES OF lt_return INTO TABLE rt_return.
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>error ] ).
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>warning ] ).

    lt_return = call_bupa_role_add_2( ).

    INSERT LINES OF lt_return INTO TABLE rt_return.
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>error ] ).
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>warning ] ).

    lt_return = call_replication_retail_to_oil( iv_action = lv_action_replicate ).

    INSERT LINES OF lt_return INTO TABLE rt_return.
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>error ] ).
    CHECK NOT line_exists( rt_return[ type = if_xo_const_message=>warning ] ).

    " Cadastrado realizado com sucesso
    rt_return = VALUE #( BASE rt_return ( type       = if_xo_const_message=>success
                                          id         = gc_message-id
                                          number     = gc_message-no_002 ) ).

  ENDMETHOD.


  METHOD fill_centraldataperson.

    SPLIT gs_motora-Nome AT space INTO rs_result-firstname rs_result-lastname.

    IF sy-subrc NE 0.
      rs_result-lastname = gs_motora-Nome.
    ENDIF.

  ENDMETHOD.


  METHOD call_bupa_create_from_data.

    FREE: gv_result, gt_return.

* ---------------------------------------------------------------------------
* Solução antiga: Cria BP de Motorista no ambiente atual (OIL)
* ---------------------------------------------------------------------------
*    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS'
*      STARTING NEW TASK 'ZMOTORISTAS'
*      CALLING task_finish ON END OF TASK
*      EXPORTING
*        is_centraldataperson = is_centraldataperson
*        iv_processo          = 1
**      CHANGING
*        cs_motora            = gs_motora.
*
*    WAIT UNTIL gv_result IS NOT INITIAL.
*    CLEAR: gv_result.

* ---------------------------------------------------------------------------
* Recupera o destino RFC (RETAIL)
* ---------------------------------------------------------------------------
    me->get_parameter( IMPORTING es_parameter = DATA(ls_parameter)
                                 et_return    = rt_return ).

    IF rt_return IS NOT INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Cria BP de Motorista no ambiente remoto (RETAIL)
* ---------------------------------------------------------------------------
    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS_RETAIL'
      DESTINATION ls_parameter-retail_rfc_destination
      EXPORTING
        is_centraldataperson = is_centraldataperson
        iv_processo          = 1
        iv_partner_group     = ls_parameter-partner_group
      IMPORTING
        et_return            = rt_return
      CHANGING
        cs_motora            = gs_motora.

    me->check_valid_doc_types( EXPORTING ir_types = gs_parameter-types
                               CHANGING  ct_docs  = gt_docs ).

    CALL FUNCTION 'ZFMTM_GRAVAR_MOTORISTAS'
      STARTING NEW TASK 'GRAVAR_MOTORISTAS'
      CALLING task_finish ON END OF TASK
      EXPORTING
        is_motora  = gs_motora
        it_doc_add = gt_docs.

    WAIT UNTIL gv_result IS NOT INITIAL.
    CLEAR: gv_result.

  ENDMETHOD.


  METHOD call_bupa_tax_add.

    FREE: gv_result, gt_return.

* ---------------------------------------------------------------------------
* Solução antiga: Processo de modificação do BP (OIL)
* ---------------------------------------------------------------------------
*    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS'
*      STARTING NEW TASK 'ZMOTORISTAS'
*      CALLING task_finish ON END OF TASK
*      EXPORTING
*        is_centraldataperson = gs_motora-bp
*        iv_processo          = 2
*        it_docs              = gt_docs
*      CHANGING
*        cs_motora            = gs_motora.
*
*    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.
*    CLEAR: gv_result.

* ---------------------------------------------------------------------------
* Recupera o destino RFC (RETAIL)
* ---------------------------------------------------------------------------
    me->get_parameter( IMPORTING es_parameter = DATA(ls_parameter)
                                 et_return    = rt_return ).

    IF rt_return IS NOT INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Processo de modificação do BP (RETAIL)
* ---------------------------------------------------------------------------
    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS_RETAIL'
      DESTINATION ls_parameter-retail_rfc_destination
      EXPORTING
        iv_processo      = 2
        iv_partner_group = ls_parameter-partner_group
      IMPORTING
        et_return        = rt_return
      CHANGING
        cs_motora        = gs_motora
        ct_docs          = gt_docs.

    me->check_valid_doc_types( EXPORTING ir_types = gs_parameter-types
                               CHANGING  ct_docs  = gt_docs ).

    CALL FUNCTION 'ZFMTM_GRAVAR_MOTORISTAS'
      STARTING NEW TASK 'GRAVAR_MOTORISTAS'
      CALLING task_finish ON END OF TASK
      EXPORTING
        is_motora  = gs_motora
        it_doc_add = gt_docs.

    WAIT UNTIL gv_result IS NOT INITIAL.
    CLEAR: gv_result.

  ENDMETHOD.


  METHOD call_bupa_role_add_2.

    DATA: ls_prr TYPE /scmb/prr.

    FREE: gv_result, gt_return.

* ---------------------------------------------------------------------------
* Solução antiga: Adiciona Função Parceiro no BP (OIL)
* ---------------------------------------------------------------------------
*    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS'
*      STARTING NEW TASK 'ZMOTORISTAS'
*      CALLING task_finish ON END OF TASK
*      EXPORTING
*        iv_processo = 3
*      CHANGING
*        cs_motora   = gs_motora.
*
*    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.
*    CLEAR: gv_result.

* ---------------------------------------------------------------------------
* Recupera o destino RFC (RETAIL)
* ---------------------------------------------------------------------------
    me->get_parameter( IMPORTING es_parameter = DATA(ls_parameter)
                                 et_return    = rt_return ).

    IF rt_return IS NOT INITIAL.
      RETURN.
    ENDIF.


* ---------------------------------------------------------------------------
* Recupera o ID do parceiro
* ---------------------------------------------------------------------------
    SELECT SINGLE BusinessPartnerUUID
        FROM I_BusinessPartner
        INTO @DATA(lv_uuid_x16)
        WHERE BusinessPartner = @gs_motora-bp.

    IF sy-subrc NE 0.
      CLEAR lv_uuid_x16.
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera a data de validade (início e fim) do Motorista
* ---------------------------------------------------------------------------
    SELECT SINGLE prr_guid, prr_id, valid_from, valid_to
           FROM /scmb/prr
           INTO CORRESPONDING FIELDS OF @ls_prr
           WHERE prr_guid = @lv_uuid_x16.

    IF sy-subrc NE 0.
      FREE ls_prr.
    ENDIF.

* ---------------------------------------------------------------------------
* Adiciona Função Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS_RETAIL'
      DESTINATION ls_parameter-retail_rfc_destination
      EXPORTING
        iv_processo      = 3
        iv_partner_group = ls_parameter-partner_group
        iv_create_tm     = abap_true
        is_prr           = ls_prr
      IMPORTING
        et_return        = rt_return
      CHANGING
        cs_motora        = gs_motora ##ENH_OK.

    me->check_valid_doc_types( EXPORTING ir_types = gs_parameter-types
                               CHANGING  ct_docs  = gt_docs ).

    CALL FUNCTION 'ZFMTM_GRAVAR_MOTORISTAS'
      STARTING NEW TASK 'GRAVAR_MOTORISTAS'
      CALLING task_finish ON END OF TASK
      EXPORTING
        is_motora  = gs_motora
        it_doc_add = gt_docs.

    WAIT UNTIL gv_result IS NOT INITIAL.
    CLEAR: gv_result.

  ENDMETHOD.


  METHOD task_finish.

    CASE p_task.

      WHEN 'ZMOTORISTAS'.

        RECEIVE RESULTS FROM FUNCTION 'ZFMTM_CAD_MOTORISTAS'
            IMPORTING
                cs_motora   = gs_motora
                rt_return   = gt_return.

      WHEN 'GRAVAR_MOTORISTAS'.


    ENDCASE.

    gv_result = abap_true .

  ENDMETHOD.


  METHOD call_identification_add.

    FREE: gv_result, gt_return.

* ---------------------------------------------------------------------------
* Solução antiga: Adiciona os documentos do Parceiro no BP (OIL)
* ---------------------------------------------------------------------------
*    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS'
*      STARTING NEW TASK 'ZMOTORISTAS'
*      CALLING task_finish ON END OF TASK
*      EXPORTING
*        iv_processo = 4
*        it_docs     = gt_docs
*      CHANGING
*        cs_motora   = gs_motora.
*
*    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.
*    CLEAR: gv_result.

* ---------------------------------------------------------------------------
* Recupera o destino RFC (RETAIL)
* ---------------------------------------------------------------------------
    me->get_parameter( IMPORTING es_parameter = DATA(ls_parameter)
                                 et_return    = rt_return ).

    IF rt_return IS NOT INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Adiciona os documentos do Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
    CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS_RETAIL'
      DESTINATION ls_parameter-retail_rfc_destination
      EXPORTING
        iv_processo      = 4
        iv_partner_group = ls_parameter-partner_group
        iv_operation     = iv_action
      IMPORTING
        et_return        = rt_return
      CHANGING
        cs_motora        = gs_motora
        ct_docs          = gt_docs.

    me->check_valid_doc_types( EXPORTING ir_types = gs_parameter-types
                               CHANGING  ct_docs  = gt_docs ).

    CALL FUNCTION 'ZFMTM_GRAVAR_MOTORISTAS'
      STARTING NEW TASK 'GRAVAR_MOTORISTAS'
      CALLING task_finish ON END OF TASK
      EXPORTING
        is_motora  = gs_motora
        it_doc_add = gt_docs.

    WAIT UNTIL gv_result IS NOT INITIAL.
    CLEAR: gv_result.

  ENDMETHOD.

  METHOD call_replication_retail_to_oil.

    IF iv_action = gc_action-insert.

      " Processo de replicação de dados iniciado via JOB.
      rt_return = VALUE #( BASE rt_return ( type       = if_xo_const_message=>success
                                            id         = gc_message-id
                                            number     = 009 ) ).

*      " Aguarde alguns instantes e atualize a tela.
*      rt_return = VALUE #( BASE rt_return ( type       = if_xo_const_message=>info
*                                            id         = gc_message-id
*                                            number     = 010 ) ).
      RETURN.

    ELSE.

* ---------------------------------------------------------------------------
* Recupera o destino RFC (RETAIL)
* ---------------------------------------------------------------------------
      me->get_parameter( IMPORTING es_parameter = DATA(ls_parameter)
                                   et_return    = rt_return ).

      IF rt_return IS NOT INITIAL.
        RETURN.
      ENDIF.

* ---------------------------------------------------------------------------
* Adiciona os documentos do Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
      CALL FUNCTION 'ZFMTM_CAD_MOTORISTAS_RETAIL'
        DESTINATION ls_parameter-retail_rfc_destination
        EXPORTING
          iv_processo = 6
        IMPORTING
          et_return   = rt_return
        CHANGING
          cs_motora   = gs_motora.

      CALL FUNCTION 'ZFMTM_GRAVAR_MOTORISTAS'
        STARTING NEW TASK 'GRAVAR_MOTORISTAS'
        CALLING task_finish ON END OF TASK
        EXPORTING
          is_motora  = gs_motora
          it_doc_add = gt_docs.

      WAIT UNTIL gv_result IS NOT INITIAL.
      CLEAR: gv_result.

    ENDIF.

  ENDMETHOD.


  METHOD get_parameter.

    DATA: lr_destination TYPE RANGE OF string.

    FREE: es_parameter, et_return.

* ---------------------------------------------------------------------------
* Verifica se os parâmetros já foram recuperados
* ---------------------------------------------------------------------------
    IF gs_parameter IS NOT INITIAL.
      es_parameter = gs_parameter.
      RETURN.
    ENDIF.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

* ---------------------------------------------------------------------------
* Recupera lista de destinos RFC de OIL para RETAIL
* ---------------------------------------------------------------------------
    TRY.
        lo_param->m_get_range( EXPORTING iv_modulo = gc_param_destination-modulo
                                         iv_chave1 = gc_param_destination-chave1
                                         iv_chave2 = gc_param_destination-chave2
                                         iv_chave3 = gc_param_destination-chave3
                                IMPORTING et_range = lr_destination ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_param_cx).
        FREE: lr_destination.
    ENDTRY.

    TRY.
        gs_parameter-retail_rfc_destination = lr_destination[ low = sy-mandt ]-high.

      CATCH cx_root.

        " Destino RFC para RETAIL não cadastro na tabela de parâmetros.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_005 ) ).
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera Agrupamento de parceiros de negócios
* ---------------------------------------------------------------------------
    TRY.
        lo_param->m_get_single( EXPORTING iv_modulo = gc_param_moto_agrupamento-modulo
                                          iv_chave1 = gc_param_moto_agrupamento-chave1
                                          iv_chave2 = gc_param_moto_agrupamento-chave2
                                IMPORTING ev_param  = gs_parameter-partner_group ).

      CATCH zcxca_tabela_parametros INTO lo_param_cx.

        " Agrupamento de parceiros de negócios não encontrado na tabela de parâm.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_006 ) ).
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera Tipos de documentos adicionais válidos
* ---------------------------------------------------------------------------
    TRY.
        lo_param->m_get_range( EXPORTING iv_modulo = gc_param_moto_tipo-modulo
                                         iv_chave1 = gc_param_moto_tipo-chave1
                                         iv_chave2 = gc_param_moto_tipo-chave2
                                IMPORTING et_range = gs_parameter-types ).

      CATCH zcxca_tabela_parametros INTO lo_param_cx.

        " Tipos de documento não cadastrado na tabela de parâmetros.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_011 ) ).

    ENDTRY.

* ---------------------------------------------------------------------------
* Devolve valores cadastrado na tabela de parâmetros
* ---------------------------------------------------------------------------
    es_parameter = gs_parameter.

  ENDMETHOD.


  METHOD build_reported.

    DATA: lo_dataref   TYPE REF TO data,
          ls_motorista TYPE zi_tm_motoristas,
          ls_doc_add   TYPE zi_tm_doc_add.

    FIELD-SYMBOLS: <fs_cds>  TYPE any.

    FREE: es_reported.

    LOOP AT it_return INTO DATA(ls_return).

* ---------------------------------------------------------------------------
* Determina tipo de estrutura CDS
* ---------------------------------------------------------------------------
      CASE ls_return-parameter.
        WHEN gc_cds-motoristas.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-zi_tm_motoristas.
        WHEN gc_cds-doc_add.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-zi_tm_doc_add.
        WHEN OTHERS.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-zi_tm_motoristas.
      ENDCASE.

      ASSIGN lo_dataref->* TO <fs_cds>.

* ---------------------------------------------------------------------------
* Converte mensagem
* ---------------------------------------------------------------------------
      ASSIGN COMPONENT gc_reported-msg OF STRUCTURE <fs_cds> TO FIELD-SYMBOL(<fs_msg>).

      IF sy-subrc EQ 0.
        TRY.
            <fs_msg>  = new_message( id       = ls_return-id
                                     number   = ls_return-number
                                     v1       = ls_return-message_v1
                                     v2       = ls_return-message_v2
                                     v3       = ls_return-message_v3
                                     v4       = ls_return-message_v4
                                     severity = CONV #( ls_return-type ) ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.

* ---------------------------------------------------------------------------
* Marca o campo com erro
* ---------------------------------------------------------------------------
      IF ls_return-field IS NOT INITIAL.
        ASSIGN COMPONENT |{ gc_reported-element }-{ ls_return-field }| OF STRUCTURE <fs_cds> TO FIELD-SYMBOL(<fs_field>).

        IF sy-subrc EQ 0.
          TRY.
              <fs_field> = if_abap_behv=>mk-on.
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDIF.

* ---------------------------------------------------------------------------
* Adiciona o erro na CDS correspondente
* ---------------------------------------------------------------------------
      CASE ls_return-parameter.
        WHEN gc_cds-motoristas.
          es_reported-zi_tm_motoristas[] = VALUE #( BASE es_reported-zi_tm_motoristas[] ( CORRESPONDING #( <fs_cds> ) ) ).
        WHEN gc_cds-doc_add.
          es_reported-zi_tm_doc_add[]    = VALUE #( BASE es_reported-zi_tm_doc_add[] ( CORRESPONDING #( <fs_cds> ) ) ).
        WHEN OTHERS.
          es_reported-zi_tm_motoristas[] = VALUE #( BASE es_reported-zi_tm_motoristas[] ( CORRESPONDING #( <fs_cds> ) ) ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_valid_doc_types.

    DATA(lt_docs) = ct_docs.
    FREE ct_docs.

    LOOP AT lt_docs REFERENCE INTO DATA(ls_docs).
      CHECK ls_docs->tipo IN ir_types[] AND ir_types[] IS NOT INITIAL.
      INSERT ls_docs->* INTO TABLE ct_docs.
    ENDLOOP.

  ENDMETHOD.


  METHOD save.

    FREE: gv_result, gt_return.

    CALL FUNCTION 'ZFMTM_GRAVAR_MOTORISTAS'
      STARTING NEW TASK 'GRAVAR_MOTORISTAS'
      CALLING task_finish ON END OF TASK
      EXPORTING
        is_motora  = is_motora
        it_doc_add = it_doc_add.

    WAIT UNTIL gv_result IS NOT INITIAL.
    CLEAR: gv_result.

  ENDMETHOD.


  METHOD valida_motorista.

    FREE: et_return.

* ---------------------------------------------------------------------------
* Valida Funcionário Rede SIM
* ---------------------------------------------------------------------------
    IF iv_operation = gc_action-insert AND is_motorista-BusinessPartnerRedeSim IS NOT INITIAL.

      " Não é permitido criar um novo funcionário Sim Rede.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_016
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-businesspartnerredesim ) ).

      " Apenas é permitido modificar. Procure pelo motorista utilizando o CPF.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_017
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-businesspartnerredesim ) ).
      RETURN.

    ENDIF.

* ---------------------------------------------------------------------------
* Valida Nome
* ---------------------------------------------------------------------------
    DATA(lv_nome_completo) = is_motorista-nome.
    SHIFT lv_nome_completo LEFT DELETING LEADING space.

    SPLIT lv_nome_completo AT space INTO DATA(lv_nome) DATA(lv_sobrenome).

    IF lv_nome IS INITIAL.

      " Favor preencher campo em branco.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_013
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-nome ) ).
    ELSEIF lv_sobrenome IS INITIAL.

      " Favor informar o nome e sobrenome.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_018
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-nome ) ).

    ENDIF.

* ---------------------------------------------------------------------------
* Valida CPF
* ---------------------------------------------------------------------------
    IF is_motorista-Cpf IS INITIAL.

      " Favor preencher campo em branco.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_013
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-cpf ) ).

    ENDIF.

    IF iv_operation = gc_action-insert AND is_motorista-Cpf IS NOT INITIAL.

      " Verifica se o parceiro já existe.
      SELECT partner, partnername, partner_guid, cnpj, cpf, ie, rg, cnh
        FROM zi_tm_motorista_docs
        WHERE cpf = @is_motorista-Cpf
          AND cpf IS NOT INITIAL
        INTO TABLE @DATA(lt_motorista).

      IF sy-subrc EQ 0.

        " Parceiro &1 encontrado com o CPF informado.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_015
                                              message_v1 = |{ VALUE #( lt_motorista[ 1 ]-partner OPTIONAL ) ALPHA = OUT }| ) ).
      ENDIF.

    ENDIF.

    IF is_motorista-Cpf IS NOT INITIAL AND strlen( is_motorista-Cpf ) < 11.

      " CPF &1 deve ter o comprimento &2.
      et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                            id         = gc_message-id
                                            number     = gc_message-no_019
                                            message_v1 = is_motorista-Cpf
                                            message_v2 = 11
                                            parameter  = gc_cds-motoristas
                                            field      = gc_field-cpf ) ).

    ENDIF.

* ---------------------------------------------------------------------------
* Valida CNH
* ---------------------------------------------------------------------------
    IF is_motorista-Cnh IS INITIAL.

      " Favor preencher campo em branco.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_013
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-cnh ) ).

    ENDIF.

    IF is_motorista-Validadecnh IS INITIAL.

      " Favor preencher campo em branco.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_013
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-Validadecnh ) ).

    ENDIF.

    IF is_motorista-Categoriacnh IS INITIAL.

      " Favor preencher campo em branco.
      et_return = VALUE #( BASE et_return ( type      = if_xo_const_message=>error
                                            id        = gc_message-id
                                            number    = gc_message-no_013
                                            parameter = gc_cds-motoristas
                                            field     = gc_field-Categoriacnh ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
