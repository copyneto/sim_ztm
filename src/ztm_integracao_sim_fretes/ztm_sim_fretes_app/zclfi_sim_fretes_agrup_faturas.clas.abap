CLASS zclfi_sim_fretes_agrup_faturas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      executar
        IMPORTING
          iv_ctcnumero      TYPE numc10
          iv_ctcserie       TYPE numc4
          iv_dataBasePopUp  TYPE dzfbdt
          iv_condPgtoPopUp  TYPE dzterm
          iv_formaPgtoPopUp TYPE schzw_bseg
        RETURNING
          VALUE(rt_return)  TYPE bapiret2_t.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      posting_interface_start
        RETURNING
          VALUE(rt_return) TYPE bapiret2_t,

      posting_interface_clearing
        IMPORTING
          iv_dataBasePopUp  TYPE dzfbdt
          iv_condPgtoPopUp  TYPE dzterm
          iv_formaPgtoPopUp TYPE schzw_bseg
        RETURNING
          VALUE(rt_return)  TYPE bapiret2_t,

      posting_interface_end
        RETURNING
          VALUE(rt_return) TYPE bapiret2_t,

      update_int_fre_cte
        IMPORTING
          iv_ctcnumero       TYPE zttm_int_fre_cte-ctc_numero
          iv_ctcserie        TYPE zttm_int_fre_cte-ctc_serie
          iv_doccontabilcomp TYPE zttm_int_fre_cte-doc_contabil_comp,

      get_ftclear
        RETURNING
          VALUE(rt_return) TYPE feb_t_ftclear,

      get_ftpost
        IMPORTING
          iv_dataBasePopUp  TYPE dzfbdt
          iv_condPgtoPopUp  TYPE dzterm
          iv_formaPgtoPopUp TYPE schzw_bseg
        RETURNING
          VALUE(rt_return)  TYPE feb_t_ftpost.


    CONSTANTS:
      gc_success TYPE bapi_mtype VALUE 'S',
      gc_error   TYPE bapi_mtype VALUE 'E'.
ENDCLASS.



CLASS zclfi_sim_fretes_agrup_faturas IMPLEMENTATION.
  METHOD executar.
    APPEND LINES OF posting_interface_start( ) TO rt_return.
    IF NOT line_exists( rt_return[ type = gc_error ] ). "#EC CI_STDSEQ
      APPEND LINES OF posting_interface_clearing(
        iv_dataBasePopUp  = iv_dataBasePopUp
        iv_condPgtoPopUp  = iv_condPgtoPopUp
        iv_formaPgtoPopUp = iv_formaPgtoPopUp
      ) TO rt_return.
      IF NOT line_exists( rt_return[ type = gc_error ] ). "#EC CI_STDSEQ
        DATA(ls_message) = VALUE #( rt_return[ type = gc_success ] OPTIONAL ). "#EC CI_STDSEQ
        IF ls_message-message_v1 IS NOT INITIAL AND
           ls_message-message_v2 IS NOT INITIAL.
          update_int_fre_cte(
            iv_ctcnumero       = iv_ctcnumero
            iv_ctcserie        = iv_ctcserie
            iv_doccontabilcomp = ls_message-message_v1 && ls_message-message_v2 && sy-datum(4)
          ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_ftpost.
    CONSTANTS:
      lc_tpdoc_compensacao TYPE bkpf-blart  VALUE 'KF',
      lc_bktxt             TYPE bkpf-bktxt  VALUE 'FATURA FRETE',
      lc_waers             TYPE bkpf-waers  VALUE 'BRL',
      lc_rf05a_newbs       TYPE rf05a-newbs VALUE '31'.

    DATA:
      lv_bkpf_bukrs        TYPE bkpf-bukrs,  "selecionar  BKPF-BUKRS (EMPRESA)
      lv_bseg_bupla        TYPE bseg-bupla,  "*bupla = SELECT T001Z-PAVAL, aonde T001Z-PARTY=J_1BBR e T001Z-BUKRS=EMPRESA
      lv_conta_compensacao TYPE rf05a-newko, "RF05A-NEWKO= SELECT LFA1-LIFNR,
                                             "utilizando a informação RAIZ CNPJ identificada anteriormente
                                             "nas regras de seleção de documentos + “0001” e buscar no campo LFA1-STCD1.

      lv_cobl_gsber        TYPE cobl-gsber,  "gsber = SELECT T001Z-PAVAL, aonde T001Z-PARTY=J_1BBR e T001Z-BUKRS=EMPRESA
      lv_tot_wrbtr         TYPE wrbtr.       "*wrbtr = SOMAR VALORES DAS FATURAS SELECIONADAS

    rt_return = VALUE #(
      ( stype = 'K' count = '001' fnam = gc_field_bkpf_budat  fval = |{ sy-datum DATE = USER }| )
      ( stype = 'K' count = '001' fnam = gc_field_bkpf_bldat  fval = |{ sy-datum DATE = USER }| )
      ( stype = 'K' count = '001' fnam = gc_field_bkpf_blart  fval = lc_tpdoc_compensacao       )
      ( stype = 'K' count = '001' fnam = gc_field_bkpf_bukrs  fval = lv_bkpf_bukrs              )
      ( stype = 'K' count = '001' fnam = gc_field_bkpf_waers  fval = lc_waers                   )
      ( stype = 'K' count = '001' fnam = gc_field_bkpf_bktxt  fval = lc_bktxt                   )
      ( stype = 'P' count = '002' fnam = gc_field_bseg_bupla  fval = lv_bseg_bupla              )
      ( stype = 'P' count = '002' fnam = gc_field_rf05a_newbs fval = lc_rf05a_newbs             )
      ( stype = 'P' count = '002' fnam = gc_field_rf05a_newko fval = lv_conta_compensacao       )
      ( stype = 'P' count = '002' fnam = gc_field_bseg_wrbtr  fval = |{ lv_tot_wrbtr NUMBER = USER }| )
      ( stype = 'P' count = '002' fnam = gc_field_bseg_valut  fval = |{ sy-datum DATE = USER }| )
      ( stype = 'P' count = '002' fnam = gc_field_cobl_gsber  fval = lv_cobl_gsber              )
      ( stype = 'P' count = '002' fnam = gc_field_bseg_zlsch  fval = iv_formaPgtoPopUp          )
      ( stype = 'P' count = '002' fnam = gc_field_bseg_zterm  fval = iv_condPgtoPopUp           )
      ( stype = 'P' count = '002' fnam = gc_field_bseg_zfbdt  fval = |{ iv_dataBasePopUp DATE = USER }| )


    ).
  ENDMETHOD.

  METHOD get_ftclear.
    CONSTANTS:
      lc_koart TYPE koart      VALUE 'D',
      lc_agums TYPE agums      VALUE 'F',
      lc_selfd TYPE fld30_f05a VALUE 'BELNR'.

*  Para isso, concatenar a informação de MIRO+ANO e
*  buscar esta informação na BKPF campo AWKEY e
*  retornar com BKPF-BELNR, BKPF-GJAHR e BKPF-BUKRS.
*  Com as chaves encontradas, buscar BSIK-BUZEI.
*  A chave do documento contábil que será usado na BAPI no campo SELVON
*  será os campos BKPF-BELNR+BKPF-GJAHR+BSIK-BUZEI concatenados.
"pendencia funcional para buscar miro da OF

    APPEND VALUE #(
      agkoa = lc_koart
      xnops = abap_true
      selfd = lc_selfd
**      agums = lc_agums
*      agkon = <fs_s_document>-hkont
*      agbuk = <fs_s_document>-empresa
*      selvon = |{ <fs_s_document>-numdoc }{ <fs_s_document>-anodoc }{ <fs_s_document>-item }|
*      selbis = |{ <fs_s_document>-numdoc }{ <fs_s_document>-anodoc }{ <fs_s_document>-item }|
    ) TO rt_return.


  ENDMETHOD.

  METHOD posting_interface_clearing.
    DATA:
      lt_blntab TYPE STANDARD TABLE OF blntab,
      lt_fttax  TYPE STANDARD TABLE OF fttax.

    CONSTANTS:
      lc_auglv   TYPE auglv      VALUE 'UMBUCHNG',
      lc_tcode   TYPE syst_tcode VALUE 'FB05',
      lc_sgfunct TYPE sgfunct_pi VALUE 'C'.

    DATA(lt_ftclear) = get_ftclear( ).
    DATA(lt_ftpost) = get_ftpost(
      iv_dataBasePopUp  = iv_dataBasePopUp
      iv_condPgtoPopUp  = iv_condPgtoPopUp
      iv_formapgtopopup = iv_formapgtopopup
    ).

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = lc_auglv
        i_tcode                    = lc_tcode
        i_sgfunct                  = lc_sgfunct
      IMPORTING
        e_msgid                    = sy-msgid
        e_msgno                    = sy-msgno
        e_msgty                    = sy-msgty
        e_msgv1                    = sy-msgv1
        e_msgv2                    = sy-msgv2
        e_msgv3                    = sy-msgv3
        e_msgv4                    = sy-msgv4
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
      EXCEPTIONS ##FM_SUBRC_OK
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty
      NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO DATA(lv_message).

      APPEND VALUE #(
        type       = sy-msgty
        id         = sy-msgid
        number     = sy-msgno
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
        message    = lv_message
      ) TO rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD posting_interface_start.
    DATA:
      lv_function TYPE funct_pi VALUE 'C',
      lv_mode     TYPE allgazmd VALUE 'N',
      lv_update   TYPE allgvbmd VALUE 'S'.

    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function         = lv_function
        i_mode             = lv_mode
        i_update           = lv_update
        i_user             = sy-uname
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        OTHERS             = 6.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty
      NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO DATA(lv_message).

      APPEND VALUE #(
        type       = sy-msgty
        id         = sy-msgid
        number     = sy-msgno
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
        message    = lv_message
      ) TO rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD posting_interface_end.
    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = abap_true
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty
      NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO DATA(lv_message).

      APPEND VALUE #(
        type       = sy-msgty
        id         = sy-msgid
        number     = sy-msgno
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
        message    = lv_message
      ) TO rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD update_int_fre_cte.
    UPDATE zttm_int_fre_cte
    SET doc_contabil_comp = @iv_doccontabilcomp
    WHERE ctc_numero = @iv_ctcnumero
      AND ctc_serie  = @iv_ctcserie.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
