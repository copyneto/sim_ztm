FUNCTION zfmtm_cad_motoristas_retail ##ENH_OK.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_CENTRALDATAPERSON) TYPE  BAPIBUS1006_CENTRAL_PERSON
*"       OPTIONAL
*"     VALUE(IV_PROCESSO) TYPE  I OPTIONAL
*"     VALUE(IV_PARTNER_GROUP) TYPE  BAPIBUS1006_HEAD-PARTN_GRP
*"       OPTIONAL
*"     VALUE(IV_OPERATION) TYPE  CHAR01 DEFAULT 'I'
*"     VALUE(IV_CREATE_TM) TYPE  CHAR01 DEFAULT ABAP_TRUE
*"     VALUE(IS_PRR) TYPE  /SCMB/PRR OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"  CHANGING
*"     VALUE(CS_MOTORA) TYPE  ZI_TM_MOTORISTAS OPTIONAL
*"     VALUE(CT_DOCS) TYPE  ZCTGTM_DOC_ADD OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS:
    BEGIN OF gc_color,
      new      TYPE i VALUE 0,
      negative TYPE i VALUE 1,
      critical TYPE i VALUE 2,
      positive TYPE i VALUE 3,
    END OF gc_color,

    BEGIN OF gc_operation,
      insert TYPE c VALUE 'I',
      update TYPE c VALUE 'U',
    END OF gc_operation,

    BEGIN OF gc_snro,
      nr_range_nr TYPE nrnr VALUE '01',
      object      TYPE nrobj  VALUE 'ZTM_MOTO',
    END OF gc_snro.

  DATA:
    lt_doc_add     TYPE STANDARD TABLE OF zttm_doc_add,
    lt_return      TYPE bapiret2_t,
    ls_centraldata TYPE bapibus1006_central,
    lv_bp          TYPE zttm_motoristas-bp,
    lv_bpext       TYPE bapibus1006_head-bpartner.

  FREE: et_return.

  CASE iv_processo.

* ---------------------------------------------------------------------------
* Processo de criação de BP (RETAIL)
* ---------------------------------------------------------------------------
    WHEN 1.

      " Verifica se o parceiro vindo do OIL já não existe no RETAIL.
      SELECT partner, partnername, partner_guid, cnpj, cpf, ie, rg, cnh
        FROM zi_tm_motorista_docs
        WHERE cpf = @cs_motora-Cpf
          AND cpf IS NOT INITIAL
        INTO TABLE @DATA(lt_motorista).

      IF sy-subrc EQ 0.

        SORT lt_motorista BY partner DESCENDING.

        " Parceiro &1 encontrado com o CPF informado. Preparando para atualizar.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>success
                                              id         = zcltm_cadastrar_motorista=>gc_message-id
                                              number     = zcltm_cadastrar_motorista=>gc_message-no_007
                                              message_v1 = cs_motora-bp ) ).

*        cs_motora-id     = lt_motorista[ 1 ]-partner_guid.
        cs_motora-bp     = lt_motorista[ 1 ]-partner.
        cs_motora-nome   = lt_motorista[ 1 ]-partnername.
        cs_motora-status = space.

      ELSE.

        " Cria novo Parceiro de Negócio
        CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
          EXPORTING
            partnercategory   = '1'
            centraldata       = ls_centraldata
            centraldataperson = is_centraldataperson
            partnergroup      = iv_partner_group
          IMPORTING
            businesspartner   = cs_motora-bp
          TABLES
            return            = et_return.

        IF NOT line_exists( et_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

*          SELECT SINGLE partner_guid
*            FROM but000
*            INTO @DATA(lv_partner_id)
*            WHERE partner = @cs_motora-bp.
*
*          IF sy-subrc EQ 0.
*            cs_motora-id = lv_partner_id.
*          ENDIF.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( et_return[ type = 'E' ]-message OPTIONAL ).
        ENDIF.

      ENDIF.

* ---------------------------------------------------------------------------
* Processo de modificação do BP (RETAIL)
* ---------------------------------------------------------------------------
    WHEN 2.

      " Recupera os documentos antigos
      SELECT partner, taxtype, taxnum, taxnumxl
          FROM dfkkbptaxnum
          WHERE partner EQ @cs_motora-bp
          INTO TABLE @DATA(lt_taxnum).

      IF sy-subrc EQ 0.
        SORT lt_taxnum BY partner taxtype taxnum.
      ENDIF.

      DO 2 TIMES.

        DATA lv_businesspartner TYPE bapibus1006_head-bpartner.
        DATA lv_taxtype TYPE bapibus1006tax-taxtype.
        DATA lv_taxnumber TYPE bapibus1006tax-taxnumber.

        lv_businesspartner = cs_motora-bp.

        lv_taxtype   = COND #( WHEN sy-index EQ 1
                               THEN 'BR5'           " RG
                               ELSE 'BR2' ).        " CPF

        lv_taxnumber = COND #( WHEN sy-index EQ 1
                               THEN cs_motora-Rg
                               ELSE cs_motora-Cpf ).

        TRY.
            DATA(ls_taxnum) = lt_taxnum[ partner = cs_motora-bp taxtype = lv_taxtype ].
          CATCH cx_root.
            CLEAR ls_taxnum.
        ENDTRY.

        DATA(lv_operation) = COND #( WHEN ls_taxnum-taxnum IS INITIAL
                                     THEN gc_operation-insert
                                     WHEN ls_taxnum-taxnum NE lv_taxnumber
                                     THEN gc_operation-update
                                     ELSE space ).

        IF lv_operation EQ gc_operation-insert.

          CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
            EXPORTING
              businesspartner = lv_businesspartner
              taxtype         = lv_taxtype
              taxnumber       = lv_taxnumber
            TABLES
              return          = et_return.

        ELSEIF lv_operation EQ gc_operation-update.

          CALL FUNCTION 'BAPI_BUPA_TAX_CHANGE'
            EXPORTING
              businesspartner = lv_businesspartner
              taxtype         = lv_taxtype
              taxnumber       = lv_taxnumber
            TABLES
              return          = et_return.

        ELSE.

          " Nenhuma operação necessária
          CONTINUE.

        ENDIF.

        IF NOT line_exists( et_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( et_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.

        ENDIF.

      ENDDO.

* ---------------------------------------------------------------------------
* Adiciona Função Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
    WHEN 3.

      IF iv_create_tm EQ abap_true.

        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner     = cs_motora-bp
            businesspartnerrole = 'TM0001'
            validfromdate       = '19000101'
            validuntildate      = '20991231'
          TABLES
            return              = et_return.

        IF NOT line_exists( et_return[ type = 'E' ] ).

          SELECT SINGLE BusinessPartnerUUID
            FROM I_BusinessPartner
            INTO @DATA(lv_uuid_x16)
            WHERE BusinessPartner = @cs_motora-bp.

          DATA(lt_prr) = VALUE /scmb/tt_prr_upd( ( prr_guid   = lv_uuid_x16
                                                   prr_id     = cs_motora-bp
                                                   calendar   = COND #( WHEN is_prr-calendar IS NOT INITIAL
                                                                        THEN is_prr-calendar
                                                                        ELSE 'BR' )
                                                   valid_from = COND #( WHEN is_prr IS NOT INITIAL
                                                                        THEN is_prr-valid_from
                                                                        ELSE '19000101000000' )
                                                   valid_to   = COND #( WHEN is_prr IS NOT INITIAL
                                                                        THEN is_prr-valid_to
                                                                        ELSE '20991231000000' )
                                                   mode       = 'I' ) ).

          CALL FUNCTION '/SCMB/PRR_UPD'
            EXPORTING
              iv_tcode = sy-tcode
              it_prr   = lt_prr.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        ELSE.

          " A função PN &1 já existe para o parceiro &2
          IF line_exists( et_return[ type = 'E' id = 'R11' number = '653' ] ).

            FREE et_return.

            SELECT SINGLE BusinessPartnerUUID
              FROM I_BusinessPartner
              INTO @lv_uuid_x16
              WHERE BusinessPartner = @cs_motora-bp.

            lt_prr = VALUE /scmb/tt_prr_upd( ( prr_guid   = lv_uuid_x16
                                               prr_id     = cs_motora-bp
                                               calendar   = COND #( WHEN is_prr-calendar IS NOT INITIAL
                                                                    THEN is_prr-calendar
                                                                    ELSE 'BR' )
                                               valid_from = COND #( WHEN is_prr-valid_from IS NOT INITIAL
                                                                    THEN is_prr-valid_from
                                                                    ELSE '19000101000000' )
                                               valid_to   = COND #( WHEN is_prr-valid_to IS NOT INITIAL
                                                                    THEN is_prr-valid_to
                                                                    ELSE '20991231000000' )
                                               mode       = 'M' ) ).

            CALL FUNCTION '/SCMB/PRR_UPD'
              EXPORTING
                iv_tcode = sy-tcode
                it_prr   = lt_prr.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.

            EXIT.
          ELSE.
            cs_motora-status = VALUE #( et_return[ type = 'E' ]-message OPTIONAL ).
            EXIT.
          ENDIF.

        ENDIF.

      ENDIF.

* ---------------------------------------------------------------------------
* Adiciona os documentos do Parceiro no BP (RETAIL)
* ---------------------------------------------------------------------------
    WHEN 4.

      DATA lv_identificationcategory TYPE c LENGTH 6.
      DATA lt_docs_add_new TYPE zctgtm_doc_add.
      DATA lv_timestamp TYPE timestampl.

      " Recupera os documentos antigos
      SELECT partner, type, idnumber, institute, entry_date, valid_date_from, valid_date_to, country, region
          FROM but0id
          WHERE partner EQ @cs_motora-bp
          INTO TABLE @DATA(lt_old).

      " Remove os documentos antigos
      LOOP AT lt_old REFERENCE INTO DATA(ls_old).

        " Se existir o documento antigo, remover.
        IF ( line_exists( ct_docs[ tipo = ls_old->type ] ) )
           OR ( ls_old->type = 'ZCNH' AND cs_motora-Cnh IS NOT INITIAL ).

          CALL FUNCTION 'BAPI_IDENTIFICATION_REMOVE'
            EXPORTING
              businesspartner        = ls_old->partner
              identificationcategory = ls_old->type
              identificationnumber   = ls_old->idnumber
            TABLES
              return                 = lt_return.

          IF NOT line_exists( lt_return[ type = 'E' ] ).

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.

          ELSE.

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          ENDIF.

        ELSEIF iv_operation = gc_operation-insert.

          GET TIME STAMP FIELD lv_timestamp.

          " Caso seja um processo de criação no OIL, precisamos adicionar os documentos do RETAIL no cadastro de motorista.
          TRY.
              lt_docs_add_new = VALUE #( BASE lt_docs_add_new ( id                    = cs_motora-id
                                                                paiid                 = cl_system_uuid=>create_uuid_x16_static( )
                                                                bp                    = cs_motora-bp
                                                                cpf                   = space
                                                                tipo                  = ls_old->type
                                                                numero                = ls_old->idnumber
                                                                numerocriticality     = 1
                                                                validadedesde         = ls_old->valid_date_from
                                                                validade              = ls_old->valid_date_to
                                                                uf                    = ls_old->region
                                                                descricao             = ls_old->institute
                                                                created_by            = sy-uname
                                                                created_at            = lv_timestamp
                                                                last_changed_by       = sy-uname
                                                                last_changed_at       = lv_timestamp
                                                                local_last_changed_at = lv_timestamp
                                                                ) ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.

      ENDLOOP.

      DO lines( ct_docs ) + 1 TIMES.

        TRY.
            DATA(ls_docs) = ct_docs[ sy-index ].
          CATCH cx_root.
            CLEAR ls_docs.
        ENDTRY.

        lv_identificationcategory     = COND #( WHEN sy-index EQ lines( ct_docs ) + 1
                                                THEN 'ZCNH'
                                                ELSE ls_docs-tipo ).

        ls_docs-numero                = COND #( WHEN ls_docs IS NOT INITIAL AND ls_docs-numero IS NOT INITIAL
                                                THEN ls_docs-numero
                                                WHEN ls_docs IS NOT INITIAL AND ls_docs-numero IS INITIAL
                                                THEN '00000000'
                                                ELSE space ).

        DATA(lv_identificationnumber) = COND bu_id_number( WHEN sy-index EQ lines( ct_docs ) + 1
                                                           THEN cs_motora-Cnh
                                                           ELSE ls_docs-numero ).

        IF lv_identificationnumber IS INITIAL.
          " Tipo &1 sem número de documento.
          et_return[] = VALUE #( BASE et_return[] ( type = 'E' id = 'ZTM_MOTORISTAS' number = '003' message_v1 = VALUE #( ct_docs[ sy-index ]-tipo OPTIONAL ) ) ).
          EXIT.
        ENDIF.

        DATA(ls_identification) = VALUE bapibus1006_identification( identrydate     = sy-datum
                                                                    idvalidfromdate = COND #( WHEN sy-index EQ lines( ct_docs ) + 1
                                                                                              THEN sy-datum
                                                                                              ELSE ls_docs-validadedesde )
                                                                    idvalidtodate   = COND #( WHEN sy-index EQ lines( ct_docs ) + 1
                                                                                              THEN cs_motora-Validadecnh
                                                                                              ELSE ls_docs-validade )
                                                                    idinstitute     = COND #( WHEN sy-index EQ lines( ct_docs ) + 1
                                                                                              THEN cs_motora-Categoriacnh
                                                                                              ELSE ls_docs-descricao )
                                                                    country         = COND #( WHEN lv_identificationcategory NE 'ZCNH'
                                                                                              THEN 'BR'
                                                                                              ELSE space )
                                                                    region          = COND #( WHEN lv_identificationcategory NE 'ZCNH'
                                                                                              THEN ls_docs-uf
                                                                                              ELSE space )
                                                                  ).

        CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
          EXPORTING
            businesspartner        = cs_motora-bp
            identificationcategory = lv_identificationcategory
            identificationnumber   = lv_identificationnumber
            identification         = ls_identification
          TABLES
            return                 = et_return.


        IF NOT line_exists( et_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( et_return[ type = 'E' ]-message OPTIONAL ).

        ENDIF.

        IF ls_docs IS NOT INITIAL.
          lt_doc_add = VALUE #( BASE lt_doc_add ( CORRESPONDING #( ls_docs ) ) ).
        ENDIF.

      ENDDO.

      " Adiciona os documentos já existentes no RETAIL na lista do Aplicativo OIL
      INSERT LINES OF lt_docs_add_new INTO TABLE ct_docs.

      DATA: lv_partner TYPE bu_partner,
            lv_tax     TYPE bus_tax_common,
            lv_save    TYPE boole-boole.

      lv_partner = cs_motora-bp.
      lv_tax = abap_true.
      lv_save = abap_true.

      TRY.
          CALL FUNCTION 'BUPA_TAX_COMMON_CHANGE'
            EXPORTING
              iv_partner    = lv_partner            " Business Partner Number
              iv_tax_common = lv_tax                " BP: Tax Numbers: General Data
              iv_x_save     = lv_save.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        CATCH cx_root.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ENDTRY.

* ---------------------------------------------------------------------------
* Elimina o Parceiro (RETAIL)
* ---------------------------------------------------------------------------
    WHEN 5.

      DATA lv_idnumber TYPE bapibus1006_identification_key-identificationnumber.
      LOOP AT ct_docs ASSIGNING FIELD-SYMBOL(<fs_docs>).

        lv_idnumber = <fs_docs>-numero.

        CALL FUNCTION 'BAPI_IDENTIFICATION_REMOVE'
          EXPORTING
            businesspartner        = cs_motora-bp
            identificationcategory = <fs_docs>-tipo
            identificationnumber   = lv_idnumber
          TABLES
            return                 = et_return.

        IF NOT line_exists( et_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( et_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.

        ENDIF.

      ENDLOOP.

* ---------------------------------------------------------------------------
* Processo de replicação de dados do RETAIL para OIL
* ---------------------------------------------------------------------------
    WHEN 6.

      DATA(lo_instance) = NEW zclbp_replica_dados( ).

      lo_instance->start( EXPORTING it_bp     = VALUE #( ( sign   = zclbp_replica_dados=>gc_values-i
                                                           option = zclbp_replica_dados=>gc_values-eq
                                                           low    = cs_motora-bp ) )
                          IMPORTING et_return = et_return ).

* ---------------------------------------------------------------------------
* Elimina o Parceiro (OIL)
* ---------------------------------------------------------------------------
    WHEN 7.

      DATA: lt_motora_del TYPE STANDARD TABLE OF zttm_motoristas,
            lt_doc_del    TYPE STANDARD TABLE OF zttm_doc_add.

      SELECT id, bp
          FROM zttm_motoristas
          WHERE bp = @cs_motora-bp
          INTO CORRESPONDING FIELDS OF TABLE @lt_motora_del.

      IF sy-subrc NE 0.
        FREE lt_motora_del.
      ENDIF.

      IF lt_motora_del IS NOT INITIAL.

        SELECT id, paiid, tipo, numero
            FROM zttm_doc_add
            FOR ALL ENTRIES IN @lt_motora_del
            WHERE id = @lt_motora_del-id
            INTO CORRESPONDING FIELDS OF TABLE @lt_doc_del.

        IF sy-subrc NE 0.
          FREE lt_doc_del.
        ENDIF.
      ENDIF.

      IF lt_motora_del IS NOT INITIAL.
        DELETE zttm_motoristas FROM TABLE lt_motora_del.
      ENDIF.

      IF lt_doc_del IS NOT INITIAL.
        DELETE zttm_doc_add FROM TABLE lt_doc_del.
      ENDIF.

      COMMIT WORK.

  ENDCASE.

  IF cs_motora-status IS INITIAL.
    " Cadastrado realizado com sucesso
    MESSAGE s002(ztm_motoristas) INTO cs_motora-status.
  ENDIF.

ENDFUNCTION.
