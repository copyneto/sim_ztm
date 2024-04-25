CLASS lcl_TM_MOTORISTAS DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_tm_motoristas RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_tm_motoristas RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zi_tm_motoristas.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zi_tm_motoristas.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zi_tm_motoristas.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_motoristas RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_tm_motoristas.

    METHODS rba_Filho FOR READ
      IMPORTING keys_rba FOR READ zi_tm_motoristas\_Filho FULL result_requested RESULT result LINK association_links.

    METHODS cadastrar FOR MODIFY
      IMPORTING keys FOR ACTION zi_tm_motoristas~cadastrar.
*      IMPORTING keys FOR ACTION zi_tm_motoristas~cadastrar RESULT result.

    METHODS cba_filho FOR MODIFY
      IMPORTING entities_cba FOR CREATE zi_tm_motoristas\_filho.

    METHODS adicionar FOR MODIFY
      IMPORTING keys FOR ACTION zi_tm_motoristas~adicionar.

ENDCLASS.

CLASS lcl_TM_MOTORISTAS IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
         ENTITY zi_tm_motoristas
         ALL FIELDS  WITH CORRESPONDING #( keys )
         RESULT DATA(lt_result)
         FAILED DATA(lt_failed_moto)
         REPORTED DATA(lt_reported_moto).

    CHECK lt_failed_moto IS INITIAL.
    CHECK lt_reported_moto IS INITIAL.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
         ENTITY zi_tm_motoristas BY \_filho
         ALL FIELDS  WITH CORRESPONDING #( keys )
         RESULT DATA(lt_doc)
         FAILED DATA(lt_failed_doc)
         REPORTED DATA(lt_reported_doc).

    CHECK lt_failed_doc IS INITIAL.
    CHECK lt_reported_doc IS INITIAL.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_key>).

      INSERT CORRESPONDING #( <fs_key> ) INTO TABLE result ASSIGNING FIELD-SYMBOL(<fs_result>).

      DATA(ls_motorista) = VALUE #( lt_result[ KEY id %tky = <fs_key>-%tky ] OPTIONAL ).

      <fs_result>-%action-cadastrar = COND #( WHEN ls_motorista-StatusReplicacao EQ 'APP'
                                                OR line_exists( lt_doc[ StatusReplicacao = 'APP' ] )
                                              THEN if_abap_behv=>fc-o-enabled
                                              ELSE if_abap_behv=>fc-o-disabled ).

      <fs_result>-%update           = if_abap_behv=>fc-o-enabled.

      <fs_result>-%delete           = COND #( WHEN ls_motorista-StatusReplicacao EQ 'APP'
                                              THEN if_abap_behv=>fc-o-enabled
                                              ELSE if_abap_behv=>fc-o-disabled ).

      <fs_result>-%field-Nome       = COND #( WHEN ls_motorista-bp IS NOT INITIAL
                                              THEN if_abap_behv=>fc-f-read_only
                                              ELSE if_abap_behv=>fc-f-unrestricted ).

      <fs_result>-%field-Cpf        = COND #( WHEN ls_motorista-bp IS NOT INITIAL
                                              THEN if_abap_behv=>fc-f-read_only
                                              ELSE if_abap_behv=>fc-f-unrestricted ).

      <fs_result>-%action-adicionar = if_abap_behv=>fc-o-enabled.

      CLEAR: ls_motorista.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD create.

    DATA: lt_motoristas TYPE STANDARD TABLE OF zttm_motoristas,
          lt_return     TYPE bapiret2_t,
          lv_timestamp  TYPE timestampl.

* ---------------------------------------------------------------------------
* Recupera os dados da linha
* ---------------------------------------------------------------------------
    TRY.
        DATA(ls_entity) = entities[ 1 ].
      CATCH cx_root.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------------
* Adiciona Função Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
    TRY.
        DATA(ls_motora) = CORRESPONDING zi_tm_motoristas( ls_entity ).
        ls_motora-id        = cl_system_uuid=>create_uuid_x16_static( ).
        ls_motora-createdby = sy-uname.
        GET TIME STAMP FIELD ls_motora-createdat.

      CATCH cx_root.
        failed-zi_tm_motoristas = CORRESPONDING #( entities ).
        RETURN.
    ENDTRY.

    DATA(lo_motorista) = NEW zcltm_cadastrar_motorista( ).

* ---------------------------------------------------------------------------
* Valida dados do motorista
* ---------------------------------------------------------------------------
    IF lt_return IS INITIAL.
      lo_motorista->valida_motorista( EXPORTING is_motorista = ls_motora
                                      IMPORTING et_return    = lt_return ).
    ENDIF.

* ---------------------------------------------------------------------------
* Grava os dados na tabela
* ---------------------------------------------------------------------------
    IF lt_return IS INITIAL.
      lo_motorista->save( EXPORTING is_motora  = ls_motora
                          IMPORTING et_return  = lt_return ).
    ENDIF.

    lo_motorista->build_reported( EXPORTING it_return   = lt_return
                                  IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

* ---------------------------------------------------------------------------
* Muda o mapeamento
* ---------------------------------------------------------------------------
    mapped-zi_tm_motoristas = VALUE #( ( %cid = ls_entity-%cid
                                         id   = ls_motora-id
                                         ) ).



  ENDMETHOD.


  METHOD cba_Filho.

    DATA: lt_doc       TYPE zctgtm_doc_add,
          ls_doc       TYPE zstm_doc_add,
          lv_timestamp TYPE timestampl.

* ---------------------------------------------------------------------------
* Recupera os dados da linha
* ---------------------------------------------------------------------------
    TRY.
        DATA(ls_entity) = entities_cba[ 1 ].
      CATCH cx_root.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------------
* Adiciona Função Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
    TRY.

        LOOP AT ls_entity-%target REFERENCE INTO DATA(ls_target) .

          ls_doc = CORRESPONDING #( ls_target->* ).
          ls_doc-id        = ls_entity-id.
          ls_doc-paiid     = cl_system_uuid=>create_uuid_x16_static( ).
          ls_doc-created_by = sy-uname.
          GET TIME STAMP FIELD ls_doc-created_at.
          lt_doc = VALUE #( BASE lt_doc ( ls_doc ) ).

* ---------------------------------------------------------------------------
* Muda o mapeamento
* ---------------------------------------------------------------------------
          mapped-zi_tm_doc_add = VALUE #( BASE mapped-zi_tm_doc_add
                                        ( %cid  = ls_target->%cid
                                          id    = ls_doc-id
                                          paiid = ls_doc-paiid
                                          ) ).
        ENDLOOP.

      CATCH cx_root.
        failed-zi_tm_motoristas = CORRESPONDING #( entities_cba ).
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------------
* Grava os dados na tabela
* ---------------------------------------------------------------------------
    zcltm_cadastrar_motorista=>save( EXPORTING it_doc_add = lt_doc
                                     IMPORTING et_return  = DATA(lt_return) ).

  ENDMETHOD.


  METHOD update.

    DATA: lt_return     TYPE bapiret2_t,
          lt_return_all TYPE bapiret2_t,
          ls_motoristas TYPE  zi_tm_motoristas,
          lv_timestamp  TYPE timestampl.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
          ENTITY zi_tm_motoristas
          ALL FIELDS WITH CORRESPONDING #( entities )
          RESULT DATA(lt_moto)
          FAILED FINAL(lt_failed_moto)
          REPORTED DATA(lt_reported_moto).

    CHECK lt_failed_moto IS INITIAL.
    CHECK lt_reported_moto IS INITIAL.

    DELETE lt_moto WHERE StatusReplicacao = zcltm_cadastrar_motorista=>gc_action-delete.

    CHECK lt_moto IS NOT INITIAL.

    GET TIME STAMP FIELD lv_timestamp.

    DATA(lo_motorista) = NEW zcltm_cadastrar_motorista( ).

    LOOP AT entities REFERENCE INTO DATA(ls_entity).

      READ TABLE lt_moto REFERENCE INTO DATA(ls_moto) WITH KEY id    = ls_entity->id
                                                               BINARY SEARCH.
      IF sy-subrc NE 0.
        failed-zi_tm_motoristas[] = VALUE #( BASE failed-zi_tm_motoristas ( CORRESPONDING #( ls_entity->* ) ) ).
        CONTINUE.
      ENDIF.

      ls_motoristas = VALUE #( id                          = ls_entity->id
                               BusinessPartnerRedeSim      = COND #( WHEN ls_entity->%control-BusinessPartnerRedeSim IS NOT INITIAL
                                                                     THEN ls_entity->BusinessPartnerRedeSim
                                                                     ELSE ls_moto->BusinessPartnerRedeSim )
                               cpf                         = COND #( WHEN ls_entity->%control-Cpf IS NOT INITIAL
                                                                     THEN ls_entity->Cpf
                                                                     ELSE ls_moto->Cpf )
                               nome                        = COND #( WHEN ls_entity->%control-nome IS NOT INITIAL
                                                                     THEN ls_entity->nome
                                                                     ELSE ls_moto->nome )
                               rg                          = COND #( WHEN ls_entity->%control-rg IS NOT INITIAL
                                                                     THEN ls_entity->rg
                                                                     ELSE ls_moto->rg )
                               cnh                         = COND #( WHEN ls_entity->%control-cnh IS NOT INITIAL
                                                                     THEN ls_entity->cnh
                                                                     ELSE ls_moto->cnh )
                               validadecnh                 = COND #( WHEN ls_entity->%control-validadecnh IS NOT INITIAL
                                                                     THEN ls_entity->validadecnh
                                                                     ELSE ls_moto->validadecnh )
                               categoriacnh                = COND #( WHEN ls_entity->%control-categoriacnh IS NOT INITIAL
                                                                     THEN ls_entity->categoriacnh
                                                                     ELSE ls_moto->categoriacnh )
                               bp                          = COND #( WHEN ls_entity->%control-bp IS NOT INITIAL
                                                                     THEN ls_entity->bp
                                                                     ELSE ls_moto->bp )
                               status                      = COND #( WHEN ls_entity->%control-status IS NOT INITIAL
                                                                     THEN ls_entity->status
                                                                     ELSE ls_moto->status )
                               createdby                   = ls_moto->CreatedBy
                               createdat                   = ls_moto->CreatedAt
                               lastchangedby               = sy-uname
                               lastchangedat               = lv_timestamp
                               LocalLastChangedAt          = lv_timestamp
                               ).

* ---------------------------------------------------------------------------
* Valida dados do motorista
* ---------------------------------------------------------------------------
      lo_motorista->valida_motorista( EXPORTING is_motorista = ls_motoristas
                                                iv_operation = zcltm_cadastrar_motorista=>gc_action-update
                                      IMPORTING et_return    = lt_return ).

      INSERT LINES OF lt_return INTO TABLE lt_return_all.

* ---------------------------------------------------------------------------
* Grava os dados na tabela
* ---------------------------------------------------------------------------
      IF lt_return IS INITIAL.
        zcltm_cadastrar_motorista=>save( EXPORTING is_motora  = ls_motoristas
                                         IMPORTING et_return  = lt_return ).
      ENDIF.

    ENDLOOP.

    lo_motorista->build_reported( EXPORTING it_return   = lt_return_all
                                  IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD delete.

    DATA: lt_motoristas TYPE STANDARD TABLE OF zttm_motoristas.

    lt_motoristas = CORRESPONDING #( keys ).

    DELETE zttm_motoristas FROM TABLE lt_motoristas.

    IF sy-subrc NE 0.
      failed-zi_tm_motoristas   = CORRESPONDING #( keys ).
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD read.

    IF keys IS NOT INITIAL.

      SELECT id,
             BusinessPartnerRedeSim,
             BusinessPartnerRedeSimCrit,
             BusinessPartnerGrouping,
             BusinessPartnerGroupingText,
             Cpf,
             CpfCriticality,
             Nome,
             Rg,
             RgCriticality,
             Cnh,
             CnhCriticality,
             Validadecnh,
             ValidadecnhCriticality,
             Categoriacnh,
             bp,
             bpCriticality,
             status,
             CreatedBy,
             CreatedAt,
             LastChangedBy,
             LastChangedAt,
             LocalLastChangedAt,
             StatusReplicacao
          FROM zi_tm_motoristas
          FOR ALL ENTRIES IN @keys
          WHERE id = @keys-id
          INTO TABLE @DATA(lt_motoristas).

      IF sy-subrc NE 0.

        result = VALUE #( FOR ls_keys_ IN keys ( %key             = ls_keys_-%key
                                                 id               = ls_keys_-id
                                                 StatusReplicacao = zcltm_cadastrar_motorista=>gc_action-delete ) ).
        RETURN.
      ENDIF.
    ENDIF.

    SORT lt_motoristas BY id.
    result = CORRESPONDING #( lt_motoristas ).

  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD rba_Filho.

    IF keys_rba IS NOT INITIAL.

      SELECT id,
             paiid,
             Tipo,
             bp,
             Numero,
             NumeroCriticality,
             ValidadeDesde,
             Validade,
             Uf,
             Descricao,
             CreatedBy,
             CreatedAt,
             LastChangedBy,
             LastChangedAt,
             LocalLastChangedAt,
             StatusReplicacao
          FROM zi_tm_doc_add
          FOR ALL ENTRIES IN @keys_rba
          WHERE id =  @keys_rba-id
          INTO TABLE @DATA(lt_doc_add).

      IF sy-subrc NE 0.

        result = VALUE #( FOR ls_keys_ IN keys_rba ( %key             = ls_keys_-%key
                                                     id               = ls_keys_-id
                                                     StatusReplicacao = zcltm_cadastrar_motorista=>gc_action-delete ) ).
        RETURN.
      ENDIF.
    ENDIF.

    SORT lt_doc_add BY id paiid Tipo.
    result = CORRESPONDING #( lt_doc_add ).

  ENDMETHOD.


  METHOD cadastrar.

    DATA: lt_return_all TYPE bapiret2_t,
          ls_motora_c   TYPE zi_tm_motoristas,
          lt_docs_c     TYPE zctgtm_doc_add.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
          ENTITY zi_tm_motoristas ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(lt_motoristas)
          FAILED FINAL(lt_failed_moto)
          REPORTED DATA(lt_reported_moto).

    CHECK lt_failed_moto IS INITIAL.
    CHECK lt_reported_moto IS INITIAL.

    IF lt_motoristas IS INITIAL.
      RETURN.
    ENDIF.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
      ENTITY zi_tm_motoristas BY \_filho ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_doc_add)
      REPORTED DATA(lt_reported_doc).

    LOOP AT keys REFERENCE INTO DATA(ls_keys).

      READ TABLE lt_motoristas REFERENCE INTO DATA(ls_motorista) WITH KEY id = ls_keys->id BINARY SEARCH.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      DATA(lo_motorista) = NEW zcltm_cadastrar_motorista( is_motora = CORRESPONDING #( ls_motorista->* ) it_docs = CORRESPONDING #( lt_doc_add ) ).

      DATA(lt_return) = lo_motorista->execute( IMPORTING es_motora = ls_motora_c
                                                         et_docs   = lt_docs_c ).

      INSERT LINES OF lt_return INTO TABLE lt_return_all.

*      mapped-zi_tm_motoristas = VALUE #( BASE mapped-zi_tm_motoristas ( %cid  = ls_keys->%cid_ref
*                                                                        id    = ls_motora_c-id ) ).

*      result = VALUE #( BASE result (  %cid_ref  = ls_keys->%cid_ref
*                                       %key-id   = ls_motora_c-id
*                                       id        = ls_motora_c-id
*                                       %param    = CORRESPONDING #( ls_motora_c )
*                                       ) ).

    ENDLOOP.

    lo_motorista->build_reported( EXPORTING it_return   = lt_return_all
                                  IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.


  METHOD adicionar.

    DATA: lt_doc_add   TYPE zctgtm_doc_add,
          lv_timestamp TYPE timestampl,
          lt_return    TYPE bapiret2_t.

    GET TIME STAMP FIELD lv_timestamp.

    LOOP AT keys REFERENCE INTO DATA(ls_key).

      TRY.
          lt_doc_add = VALUE #( BASE lt_doc_add (
                                id                          = ls_key->id
                                paiid                       = cl_system_uuid=>create_uuid_x16_static( )
                                tipo                        = ls_key->%param-Tipo
                                numero                      = ls_key->%param-Numero
                                validadedesde               = ls_key->%param-ValidadeDesde
                                validade                    = ls_key->%param-Validade
                                uf                          = ls_key->%param-Uf
                                descricao                   = ls_key->%param-Descricao
                                created_by                  = sy-uname
                                created_at                  = lv_timestamp
                                last_changed_by             = sy-uname
                                last_changed_at             = lv_timestamp
                                local_last_changed_at       = lv_timestamp
                                ) ).
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.

* ---------------------------------------------------------------------------
* Verifica se existe outro documento do mesmo tipo adicionado
* ---------------------------------------------------------------------------
    SELECT id, paiid, tipo, numero
        FROM zi_tm_doc_add
        FOR ALL ENTRIES IN @lt_doc_add
        WHERE id    EQ @lt_doc_add-id
          AND paiid NE @lt_doc_add-paiid
          AND tipo  EQ @lt_doc_add-tipo
        INTO TABLE @DATA(lt_doc_add_c).

    IF sy-subrc EQ 0.

      " Documento do tipo &1 já foi adicionado.
      lt_return = VALUE #( BASE lt_return ( type       = if_xo_const_message=>error
                                            id         = zcltM_cadastrar_motorista=>gc_message-id
                                            number     = 014
                                            message_v1 = VALUE #( lt_doc_add_c[ 1 ]-tipo OPTIONAL )
                                            ) ).

    ENDIF.

    DATA(lo_motorista) = NEW zcltm_cadastrar_motorista( ).

* ---------------------------------------------------------------------------
* Grava os dados na tabela
* ---------------------------------------------------------------------------
    IF lt_return IS INITIAL.

      lo_motorista->save( EXPORTING it_doc_add = lt_doc_add
                                       IMPORTING et_return  = lt_return ).
    ENDIF.

    lo_motorista->build_reported( EXPORTING it_return   = lt_return
                                  IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_TM_DOC_ADD DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_tm_doc_add RESULT result.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zi_tm_doc_add.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zi_tm_doc_add.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_doc_add RESULT result.

    METHODS rba_Pai FOR READ
      IMPORTING keys_rba FOR READ zi_tm_doc_add\_Pai FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_ZI_TM_DOC_ADD IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
        ENTITY zi_tm_doc_add
          ALL FIELDS
          WITH CORRESPONDING #( keys )
          RESULT DATA(lt_doc)
          FAILED DATA(lt_failed_doc)
          REPORTED DATA(lt_reported_doc).

    CHECK lt_failed_doc IS INITIAL.
    CHECK lt_reported_doc IS INITIAL.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_key>).

      INSERT CORRESPONDING #( <fs_key> ) INTO TABLE result ASSIGNING FIELD-SYMBOL(<fs_result>).

      DATA(ls_doc) = VALUE #( lt_doc[ KEY id %tky = <fs_key>-%tky ] OPTIONAL ).

      <fs_result>-%delete        = COND #( WHEN ls_doc-StatusReplicacao EQ 'BP'
                                           THEN if_abap_behv=>fc-o-disabled
                                           ELSE if_abap_behv=>fc-o-enabled ).

      <fs_result>-%field-Numero  = if_abap_behv=>fc-f-unrestricted.

      <fs_result>-%field-Tipo    = COND #( WHEN ls_doc-StatusReplicacao EQ 'BP'
                                           THEN if_abap_behv=>fc-f-read_only
                                           ELSE if_abap_behv=>fc-f-mandatory ).

    ENDLOOP.

  ENDMETHOD.

  METHOD update.

    DATA: lt_doc_add   TYPE zctgtm_doc_add,
          lv_timestamp TYPE timestampl.

    READ ENTITIES OF zi_tm_motoristas IN LOCAL MODE
          ENTITY zi_tm_doc_add
          ALL FIELDS WITH CORRESPONDING #( entities )
          RESULT DATA(lt_add)
          FAILED FINAL(lt_failed_moto)
          REPORTED DATA(lt_reported_moto).

    CHECK lt_failed_moto IS INITIAL.
    CHECK lt_reported_moto IS INITIAL.

    GET TIME STAMP FIELD lv_timestamp.

    LOOP AT entities REFERENCE INTO DATA(ls_entity).

      READ TABLE lt_add REFERENCE INTO DATA(ls_add) WITH KEY id    = ls_entity->id
                                                             paiid = ls_entity->paiid
                                                             tipo  = ls_entity->tipo
                                                             BINARY SEARCH.
      IF sy-subrc NE 0.
        failed-zi_tm_doc_add[] = VALUE #( BASE failed-zi_tm_doc_add ( CORRESPONDING #( ls_entity->* ) ) ).
        CONTINUE.
      ENDIF.

      lt_doc_add = VALUE #( BASE lt_doc_add (
                            id                          = ls_entity->id
                            paiid                       = ls_entity->paiid
                            tipo                        = COND #( WHEN ls_entity->%control-Tipo IS NOT INITIAL
                                                                  THEN ls_entity->Tipo
                                                                  ELSE ls_add->Tipo )
                            numero                      = COND #( WHEN ls_entity->%control-Numero IS NOT INITIAL
                                                                  THEN ls_entity->Numero
                                                                  ELSE ls_add->Numero )
                            validadedesde               = COND #( WHEN ls_entity->%control-ValidadeDesde IS NOT INITIAL
                                                                  THEN ls_entity->ValidadeDesde
                                                                  ELSE ls_add->ValidadeDesde )
                            validade                    = COND #( WHEN ls_entity->%control-Validade IS NOT INITIAL
                                                                  THEN ls_entity->Validade
                                                                  ELSE ls_add->Validade )
                            uf                          = COND #( WHEN ls_entity->%control-Uf IS NOT INITIAL
                                                                  THEN ls_entity->Uf
                                                                  ELSE ls_add->Uf )
                            descricao                   = COND #( WHEN ls_entity->%control-Descricao IS NOT INITIAL
                                                                  THEN ls_entity->Descricao
                                                                  ELSE ls_add->Descricao )
                            created_by                  = ls_add->CreatedBy
                            created_at                  = ls_add->CreatedAt
                            last_changed_by             = sy-uname
                            last_changed_at             = lv_timestamp
                            local_last_changed_at       = lv_timestamp
                            ) ).

    ENDLOOP.

* ---------------------------------------------------------------------------
* Grava os dados na tabela
* ---------------------------------------------------------------------------
    DATA(lo_motorista) = NEW zcltm_cadastrar_motorista( ).

    lo_motorista->save( EXPORTING it_doc_add = lt_doc_add
                        IMPORTING et_return  = DATA(lt_return) ).

    lo_motorista->build_reported( EXPORTING it_return   = lt_return
                                  IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD delete.

    DATA: lt_doc_add TYPE STANDARD TABLE OF zttm_doc_add.

    lt_doc_add = CORRESPONDING #( keys ).

    DELETE zttm_doc_add FROM TABLE lt_doc_add.

    IF sy-subrc NE 0.
      failed-zi_tm_doc_add   = CORRESPONDING #( keys ).
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD read.

    IF keys IS NOT INITIAL.

      SELECT id,
             paiid,
             Tipo,
             bp,
             Numero,
             NumeroCriticality,
             ValidadeDesde,
             Validade,
             Uf,
             Descricao,
             CreatedBy,
             CreatedAt,
             LastChangedBy,
             LastChangedAt,
             LocalLastChangedAt,
             StatusReplicacao
          FROM zi_tm_doc_add
          FOR ALL ENTRIES IN @keys
          WHERE id    = @keys-id
            AND paiid = @keys-paiid
            AND tipo  = @keys-tipo
          INTO TABLE @DATA(lt_doc_add).

      IF sy-subrc NE 0.

        result = VALUE #( FOR ls_keys_ IN keys ( %key             = ls_keys_-%key
                                                 id               = ls_keys_-id
                                                 paiid            = ls_keys_-paiid
                                                 Tipo             = ls_keys_-Tipo
                                                 StatusReplicacao = zcltm_cadastrar_motorista=>gc_action-delete ) ).
        RETURN.
      ENDIF.
    ENDIF.

    SORT lt_doc_add BY id paiid tipo.
    result = CORRESPONDING #( lt_doc_add ).

  ENDMETHOD.

  METHOD rba_Pai.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_TM_MOTORISTAS DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_ZI_TM_MOTORISTAS IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
