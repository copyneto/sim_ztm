FUNCTION zfmtm_cad_motoristas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_CENTRALDATAPERSON) TYPE  BAPIBUS1006_CENTRAL_PERSON
*"       OPTIONAL
*"     VALUE(IV_PROCESSO) TYPE  I OPTIONAL
*"     VALUE(IT_DOCS) TYPE  ZCTGTM_DOC_ADD OPTIONAL
*"  TABLES
*"      RT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  CHANGING
*"     VALUE(CS_MOTORA) TYPE  ZI_TM_MOTORISTAS OPTIONAL
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

  FREE: rt_return.

  CASE iv_processo.

    WHEN 1.

*      DO 10 TIMES .
*
*        DATA(lv_random_num) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
*                                                  min  = 3000000
*                                                  max  = 3999999 )->get_next( ).
*        DATA lv_bpext TYPE bapibus1006_head-bpartner.
*
*        lv_bpext = lv_random_num.
*        lv_bpext = |{ lv_bpext ALPHA = OUT }|.
*
*        SELECT SINGLE id
*            FROM zttm_motoristas
*            INTO @DATA(lv_bpe)
*            WHERE bp = @lv_bpext.
*
*        IF sy-subrc IS NOT INITIAL.
*          EXIT.
*        ENDIF.
*
*      ENDDO.

      " Se BP já foi criado, pular este passo
      IF cs_motora-bp IS NOT INITIAL.
        RETURN.
      ENDIF.

      CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
        EXPORTING
          object           = gc_snro-object
        EXCEPTIONS
          foreign_lock     = 1
          object_not_found = 2
          system_failure   = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        rt_return[] = VALUE #( BASE rt_return[] ( id         = sy-msgid
                                                  type       = COND #( WHEN sy-msgty EQ 'S' THEN 'W' ELSE sy-msgty )
                                                  number     = sy-msgno
                                                  message_v1 = sy-msgv1
                                                  message_v2 = sy-msgv2
                                                  message_v3 = sy-msgv3
                                                  message_v4 = sy-msgv4 ) ).
        EXIT.
      ENDIF.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = gc_snro-nr_range_nr
          object                  = gc_snro-object
        IMPORTING
          number                  = lv_bpext
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        rt_return[] = VALUE #( BASE rt_return[] ( id         = sy-msgid
                                                  type       = COND #( WHEN sy-msgty EQ 'S' THEN 'W' ELSE sy-msgty )
                                                  number     = sy-msgno
                                                  message_v1 = sy-msgv1
                                                  message_v2 = sy-msgv2
                                                  message_v3 = sy-msgv3
                                                  message_v4 = sy-msgv4 ) ).
        EXIT.
      ELSE.
        COMMIT WORK.
      ENDIF.

      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
        EXPORTING
          object = gc_snro-object.

      lv_bpext = |{ lv_bpext ALPHA = OUT }|.

      SELECT SINGLE id
          FROM zttm_motoristas
          INTO @DATA(lv_bpe)
          WHERE bp = @lv_bpext.

      IF sy-subrc EQ 0.
        " Número BP &1 já foi utilizado no cadastro. Tente cadastrar novamente.
        rt_return[] = VALUE #( BASE rt_return[] ( type = 'E' id = 'ZTM_MOTORISTAS' number = '004' message_v1 = lv_bpext ) ).
        EXIT.
      ENDIF.

      SELECT SINGLE low
        FROM zi_ca_get_parameter
        INTO @DATA(lv_group)
        WHERE chave1 = 'MOTORISTAS'
          AND chave2 = 'AGRUPAMENTO'
          AND modulo = 'TM'.

      DATA lv_pg TYPE bapibus1006_head-partn_grp.
      lv_pg = lv_group.
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory       = '1'
          centraldata           = ls_centraldata
          centraldataperson     = is_centraldataperson
          partnergroup          = lv_pg
          businesspartnerextern = lv_bpext
        IMPORTING
          businesspartner       = cs_motora-bp
        TABLES
          return                = rt_return.

      IF NOT line_exists( rt_return[ type = 'E' ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        cs_motora-status = space.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
      ENDIF.

    WHEN 2.

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

        DATA(lv_operation) = COND #( WHEN sy-index EQ 1
                                     THEN COND char01( WHEN cs_motora-RgCriticality EQ gc_color-negative
                                                       THEN gc_operation-insert
                                                       WHEN cs_motora-RgCriticality EQ gc_color-critical
                                                       THEN gc_operation-update
                                                       ELSE space )
                                     ELSE COND char01( WHEN cs_motora-CpfCriticality EQ gc_color-negative
                                                       THEN gc_operation-insert
                                                       WHEN cs_motora-CpfCriticality EQ gc_color-critical
                                                       THEN gc_operation-update
                                                       ELSE space ) ).


        IF lv_operation EQ gc_operation-insert.

          CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
            EXPORTING
              businesspartner = lv_businesspartner
              taxtype         = lv_taxtype
              taxnumber       = lv_taxnumber
            TABLES
              return          = rt_return.

        ELSEIF lv_operation EQ gc_operation-update.

          CALL FUNCTION 'BAPI_BUPA_TAX_CHANGE'
            EXPORTING
              businesspartner = lv_businesspartner
              taxtype         = lv_taxtype
              taxnumber       = lv_taxnumber
            TABLES
              return          = rt_return.

        ELSE.

          " Nenhuma operação necessária
          CONTINUE.

        ENDIF.

        IF NOT line_exists( rt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.

        ENDIF.

      ENDDO.

    WHEN 3.

      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner     = cs_motora-bp
          businesspartnerrole = 'TM0001'
          validfromdate       = '19000101'
          validuntildate      = '99991231'
        TABLES
          return              = rt_return.

      IF NOT line_exists( rt_return[ type = 'E' ] ).

        DATA lv_uuid_x16 TYPE sysuuid_x16.

        SELECT SINGLE BusinessPartnerUUID
          FROM I_BusinessPartner
          INTO @lv_uuid_x16
          WHERE BusinessPartner = @cs_motora-bp.

        DATA(lt_prr) = VALUE /scmb/tt_prr_upd( ( prr_guid = lv_uuid_x16
                                                 prr_id = cs_motora-bp
                                                 calendar = 'BR'
                                                 valid_from = '19000101000000'
                                                 valid_to = '99991231000000'
                                                 mode = 'I' ) ).

        CALL FUNCTION '/SCMB/PRR_UPD'
          EXPORTING
            iv_tcode = sy-tcode
            it_prr   = lt_prr.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

      ELSE.

        " A função PN &1 já existe para o parceiro &2
        IF line_exists( rt_return[ type = 'E' id = 'R11' number = '653' ] ).
          FREE rt_return.
          cs_motora-status = space.
          EXIT.
        ELSE.
          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.
        ENDIF.

      ENDIF.

    WHEN 4.

      DATA lv_identificationcategory TYPE c LENGTH 6.

      " Recupera os documentos antigos
      SELECT partner, type, idnumber
          FROM but0id
          WHERE partner EQ @cs_motora-bp
          INTO TABLE @DATA(lt_old).

      " Remove os documentos antigos
      LOOP AT lt_old REFERENCE INTO DATA(ls_old).

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

      ENDLOOP.

      DO lines( it_docs ) + 1 TIMES.

        TRY.
            DATA(ls_docs) = it_docs[ sy-index ].
          CATCH cx_root.
            CLEAR ls_docs.
        ENDTRY.

        lv_identificationcategory     = COND #( WHEN sy-index EQ lines( it_docs ) + 1
                                                THEN 'ZCNH'
                                                ELSE ls_docs-tipo ).

        ls_docs-numero                = COND #( WHEN ls_docs IS NOT INITIAL AND ls_docs-numero IS NOT INITIAL
                                                THEN ls_docs-numero
                                                WHEN ls_docs IS NOT INITIAL AND ls_docs-numero IS INITIAL
                                                THEN '00000000'
                                                ELSE space ).

        DATA(lv_identificationnumber) = COND bu_id_number( WHEN sy-index EQ lines( it_docs ) + 1
                                                           THEN cs_motora-Cnh
                                                           ELSE ls_docs-numero ).

        IF lv_identificationnumber IS INITIAL.
          " Tipo &1 sem número de documento.
          rt_return[] = VALUE #( BASE rt_return[] ( type = 'E' id = 'ZTM_MOTORISTAS' number = '003' message_v1 = VALUE #( it_docs[ sy-index ]-tipo OPTIONAL ) ) ).
          EXIT.
        ENDIF.

        DATA(ls_identification) = VALUE bapibus1006_identification( idvalidfromdate = '19000101'
                                                                    idvalidtodate   = COND #( WHEN sy-index EQ lines( it_docs ) + 1
                                                                                              THEN cs_motora-Validadecnh
                                                                                              ELSE ls_docs-validade )
                                                                    idinstitute     = COND #( WHEN sy-index EQ lines( it_docs ) + 1
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
            return                 = rt_return.


        IF NOT line_exists( rt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).

        ENDIF.

        IF ls_docs IS NOT INITIAL.
          lt_doc_add = VALUE #( BASE lt_doc_add ( CORRESPONDING #( ls_docs ) ) ).
        ENDIF.

      ENDDO.



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

    WHEN 5.

      DATA lv_idnumber TYPE bapibus1006_identification_key-identificationnumber.
      LOOP AT it_docs ASSIGNING FIELD-SYMBOL(<fs_docs>).

        lv_idnumber = <fs_docs>-numero.

        CALL FUNCTION 'BAPI_IDENTIFICATION_REMOVE'
          EXPORTING
            businesspartner        = cs_motora-bp
            identificationcategory = <fs_docs>-tipo
            identificationnumber   = lv_idnumber
          TABLES
            return                 = rt_return.

        IF NOT line_exists( rt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          cs_motora-status = space.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          cs_motora-status = VALUE #( rt_return[ type = 'E' ]-message OPTIONAL ).
          EXIT.

        ENDIF.

      ENDLOOP.

  ENDCASE.


  IF cs_motora-status IS INITIAL.
    " Cadastrado realizado com sucesso
    MESSAGE s002(ztm_motoristas) INTO cs_motora-status.
  ENDIF.

  DATA(ls_motora) = CORRESPONDING zttm_motoristas( cs_motora ).

  UPDATE zttm_motoristas FROM ls_motora.

  IF lt_doc_add IS NOT INITIAL.

    UPDATE zttm_doc_add FROM TABLE lt_doc_add.

  ENDIF.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
