CLASS zcltm_edoc_br_flex_v1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_edoc_br_flex .

    TYPES:
      ty_nfe_entrada  TYPE TABLE OF zttm_nfe_entrada .
    TYPES:
      ty_nfe_saida    TYPE TABLE OF zttm_nfe_saida .
    TYPES:
      BEGIN OF ty_mkpf,
        mblnr    TYPE mkpf-mblnr,
        mjahr    TYPE mkpf-mjahr,
        le_vbeln TYPE mkpf-le_vbeln,
      END OF ty_mkpf .
    TYPES: ty_dec2 TYPE p LENGTH 8 DECIMALS 2.
    TYPES:
      ty_mkpf_t TYPE TABLE OF ty_mkpf .
    TYPES:
      BEGIN OF ty_frete,
        le_vbeln  TYPE mkpf-le_vbeln,
        doc_frete TYPE ze_doc_frete,
      END OF ty_frete .
    TYPES:
      BEGIN OF ty_vbrp,
        vbeln TYPE vbrp-vbeln,
        posnr TYPE vbrp-posnr,
        vgbel TYPE vbrp-vgbel,
      END OF ty_vbrp .
    TYPES:
      BEGIN OF ty_ekpo,
        ebeln TYPE ekpo-ebeln,
        loekz TYPE ekpo-loekz,
        txz01 TYPE ekpo-txz01,
        mwskz TYPE ekpo-mwskz,
      END OF ty_ekpo .
    TYPES:
      BEGIN OF ty_ekko,
        ebeln TYPE ekko-ebeln,
        zterm TYPE ekko-zterm,
      END OF ty_ekko .
    TYPES:
      ty_vbrp_t TYPE TABLE OF ty_vbrp .
    TYPES:
      ty_tm_item    TYPE TABLE OF bapi_incinv_create_tm_item WITH EMPTY KEY .

    DATA:
      gt_frete TYPE TABLE OF ty_frete .
    CONSTANTS gc_1 TYPE c VALUE '1' ##NO_TEXT.
    CONSTANTS:
      gc_to(2) TYPE c VALUE 'TO' ##NO_TEXT.
    CONSTANTS gc_i TYPE c VALUE 'I' ##NO_TEXT.
    CONSTANTS:
      gc_eq(2) TYPE c VALUE 'EQ' ##NO_TEXT.
    CONSTANTS:
      gc_10(2) TYPE c VALUE '10' ##NO_TEXT.
    CONSTANTS:
      gc_li(2) TYPE c VALUE 'LI' ##NO_TEXT.
    CONSTANTS:
      gc_md(2) TYPE c VALUE 'MD' ##NO_TEXT.
    CONSTANTS:
      gc_bi(2) TYPE c VALUE 'BI' ##NO_TEXT.
    CONSTANTS gc_2 TYPE c VALUE '2' ##NO_TEXT.
    CONSTANTS:
      gc_re(2) TYPE c VALUE 'RE' ##NO_TEXT.
    CONSTANTS:
      gc_cur(3) TYPE c VALUE 'BRL' ##NO_TEXT.
    CONSTANTS:
      gc_b030(4) TYPE c VALUE 'B030' ##NO_TEXT.
    CONSTANTS:
      gc_n0(2) TYPE c VALUE 'N0' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF gc_cenario,
        transferencia TYPE char1 VALUE '0',
        remessa       TYPE char1 VALUE '1',
        venda         TYPE char1 VALUE '2',
      END OF gc_cenario .
    DATA:
      gt_nfe_entrada TYPE TABLE OF  zttm_nfe_entrada .
    DATA:
      gt_nfe_saida   TYPE TABLE OF  zttm_nfe_saida .
    DATA gs_nfe_entrada TYPE zttm_nfe_entrada .
    DATA gs_nfe_saida TYPE zttm_nfe_saida .
    DATA gv_chave_nfe TYPE string .
    DATA gv_chave_cte TYPE ze_chave_cte .
    DATA gv_bukrs TYPE bukrs .
    DATA gs_headerdata TYPE j_1bcte_header .
    DATA:
      gt_itemdata    TYPE STANDARD TABLE OF bapi_incinv_create_item .
    DATA:
      gt_tm_itemdata TYPE STANDARD TABLE OF bapi_incinv_create_tm_item .
    DATA gs_itemdata TYPE bapi_incinv_create_item .
    DATA gv_vtprest TYPE string .
    DATA gv_belnr TYPE re_belnr .
    DATA gv_gjahr TYPE gjahr .
    DATA gs_entrada TYPE zttm_nfe_entrada .
    CONSTANTS gc_c7 TYPE char2 VALUE 'C7' ##NO_TEXT.
    DATA:
      gt_ekpo TYPE TABLE OF ty_ekpo .
    DATA:
      gt_ekko TYPE TABLE OF ty_ekko .
    DATA gv_versao TYPE string .
    DATA gv_dhrecbto TYPE string .
    DATA gv_dhemi TYPE string .
    CONSTANTS gc_processo TYPE char20 VALUE 'REM_RET' ##NO_TEXT.
    CONSTANTS gc_loekz TYPE eloek VALUE 'L' ##NO_TEXT.
    CONSTANTS gc_l TYPE eloek VALUE 'L' ##NO_TEXT.
    DATA gv_dzterm TYPE dzterm .
    CONSTANTS gc_000010 TYPE char6 VALUE '000010' ##NO_TEXT.
    CONSTANTS gc_000001 TYPE char6 VALUE '000001' ##NO_TEXT.

    METHODS task_finish
      IMPORTING
        !p_task TYPE clike .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gv_result TYPE char1 .
    DATA:
      gt_return TYPE TABLE OF bapiret2 .
    DATA gv_fiscalyear TYPE bapi_incinv_fld-fisc_year .
    DATA gv_invoicedocnumber TYPE bapi_incinv_fld-inv_doc_no .
    DATA gs_ctedoc TYPE j_1bcte_doc.
    DATA: gt_cteref TYPE TABLE OF j_1bcte_ref,
          gt_ctestx TYPE TABLE OF  j_1bcte_stx,
          gt_ctelin TYPE TABLE OF j_1bcte_lin.

    METHODS   define_cenario
      IMPORTING
        !io_cte           TYPE REF TO cl_edoc_br_cte_entity
      RETURNING
        VALUE(rv_cenario) TYPE char1 .
    METHODS cenario_transf
      IMPORTING
        !io_cte           TYPE REF TO cl_edoc_br_cte_entity
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS cenario_venda
      IMPORTING
        !io_cte           TYPE REF TO cl_edoc_br_cte_entity
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS retorno_docnum
      IMPORTING
        !io_cte          TYPE REF TO cl_edoc_br_cte_entity
      RETURNING
        VALUE(rv_docnum) TYPE j_1bdocnum .
    METHODS executa_bapi
      IMPORTING
        !io_cte           TYPE REF TO cl_edoc_br_cte_entity
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS grava_nfe_entrada
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS grava_nfe_saida
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS preenche_bapi
      IMPORTING
        !io_cte           TYPE REF TO cl_edoc_br_cte_entity
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS busca_tabela
      IMPORTING
        !iv_chave_cte     TYPE ze_chave_cte
      RETURNING
        VALUE(rv_cenario) TYPE char1 .
    METHODS atualiza_tabela
      IMPORTING
        !iv_cenario       TYPE char1
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS busca_frete
      IMPORTING
        !it_mkpf          TYPE ty_mkpf_t OPTIONAL
        !it_vbrp          TYPE ty_vbrp_t OPTIONAL
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab .
    METHODS preenche_nfe_entrada .
    METHODS preenche_nfe_saida .
ENDCLASS.



CLASS ZCLTM_EDOC_BR_FLEX_V1 IMPLEMENTATION.


  METHOD atualiza_tabela.

    CASE iv_cenario.
      WHEN gc_cenario-transferencia.
        LOOP AT gt_nfe_entrada ASSIGNING FIELD-SYMBOL(<fs_entrada>).
          <fs_entrada>-belnr = gv_invoicedocnumber.
          <fs_entrada>-gjahr = gv_fiscalyear.
        ENDLOOP.
        rt_bapiret = grava_nfe_entrada( ).
      WHEN gc_cenario-venda.
        LOOP AT gt_nfe_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
          <fs_saida>-belnr = gv_invoicedocnumber.
          <fs_saida>-gjahr = gv_fiscalyear.
        ENDLOOP.
        rt_bapiret = grava_nfe_saida( ).
    ENDCASE.

  ENDMETHOD.


  METHOD busca_frete.

*** MKPF preenchida  - NFE ENTRADA
*** VBRP preenchidqa - NFE SAIDA

    "Tabela interna referente ao nó root da query
    DATA: lt_trq_base_btd_id TYPE /scmtms/t_tor_q_fo_r.

    "Service Manager
    DATA(lo_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).

    "Attribute name key
    DATA(lv_att_name) = /scmtms/if_tor_c=>sc_query_attribute-root-fo_data_by_attr-trq_base_btd_id.

    "Attribute name key
    DATA(lv_att_to) = /scmtms/if_tor_c=>sc_query_attribute-root-fo_data_by_attr-tor_cat.

    "Business Object name
    DATA(lv_bo_name) = /scmtms/if_tor_c=>sc_bo_name.

    IF it_mkpf[] IS NOT INITIAL.
      "Selection Parameters
      DATA(lt_selpar) = VALUE /bobf/t_frw_query_selparam(  FOR ls_mkpf IN it_mkpf
                                                           (   attribute_name = lv_att_name
                                                               sign           = gc_i
                                                               option         = gc_eq
                                                               low            = ls_mkpf-le_vbeln ) ).
    ELSEIF it_vbrp[] IS NOT INITIAL..
      "Selection Parameters
      lt_selpar = VALUE /bobf/t_frw_query_selparam(  FOR ls_vbrp IN it_vbrp
                                                           (   attribute_name = lv_att_name
                                                               sign           = gc_i
                                                               option         = gc_eq
                                                               low            = ls_vbrp-vgbel ) ).
    ENDIF.

    "Selection Parameters
    lt_selpar = VALUE #(  BASE lt_selpar
                               (   attribute_name = lv_att_to
                                   sign           = gc_i
                                   option         = gc_eq
                                   low            = gc_to ) ).
    "Query key
    DATA(lv_query_key) = /scmtms/if_tor_c=>sc_query-root-fo_data_by_attr.

    "Execução da query
    lo_srv_mgr->query( EXPORTING iv_query_key = lv_query_key
                                 it_selection_parameters = lt_selpar
                                 iv_fill_data = abap_true
                       IMPORTING et_data = lt_trq_base_btd_id ).

    SORT lt_trq_base_btd_id BY labeltxt tor_id.
    DELETE ADJACENT DUPLICATES FROM lt_trq_base_btd_id COMPARING labeltxt tor_id.

    IF it_mkpf[] IS NOT INITIAL.
      gt_frete = VALUE #(  FOR ls_base IN lt_trq_base_btd_id
                        (      le_vbeln =  ls_base-labeltxt
                               doc_frete = ls_base-tor_id )  ).

    ELSEIF it_vbrp[] IS NOT INITIAL.
      gt_frete = VALUE #( FOR ls_base IN lt_trq_base_btd_id
                        (     le_vbeln  = ls_base-labeltxt
                              doc_frete = ls_base-tor_id )  ).
    ENDIF.

**Valida se encontrou algum registro de FRETE
    IF gt_frete[] IS INITIAL.
      "Não é possível encontrar Doc de Frete
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '001') ).
    ENDIF.

  ENDMETHOD.


  METHOD busca_tabela.

    DATA lt_nfe TYPE TABLE OF ty_ekpo.
    DATA ls_nfe TYPE ty_ekpo.

    FREE: gt_nfe_entrada[],
          gt_nfe_saida,
          lt_nfe[],
          ls_nfe.

    SELECT mandt,
           chave_nfe,
           doc_frete,
           chave_cte,
           belnr,
           gjahr,
           bukrs
      FROM zttm_nfe_entrada
      INTO TABLE @gt_nfe_entrada
      WHERE chave_cte EQ @iv_chave_cte
      .
    IF sy-subrc IS NOT INITIAL.

      SELECT mandt,
             chave_nfe,
             doc_frete,
             chave_cte,
             belnr,
             gjahr,
             bukrs
        FROM zttm_nfe_saida
        INTO TABLE @gt_nfe_saida
        WHERE chave_cte EQ @iv_chave_cte
        .
      IF sy-subrc IS NOT INITIAL.
* MENSAGEM DE ERRO
      ELSE.
        rv_cenario = gc_cenario-venda.
        LOOP AT gt_nfe_saida ASSIGNING FIELD-SYMBOL(<fs_nfe_saida>).
          ls_nfe-txz01 = <fs_nfe_saida>-doc_frete.
          APPEND ls_nfe TO lt_nfe.
          CLEAR  ls_nfe.
        ENDLOOP.

        SORT lt_nfe BY txz01.
        DELETE ADJACENT DUPLICATES FROM lt_nfe COMPARING txz01.

        IF lt_nfe[] IS NOT  INITIAL.
          SELECT ebeln, loekz, txz01, mwskz
              FROM ekpo
              INTO TABLE @gt_ekpo
              FOR ALL ENTRIES IN @lt_nfe
              WHERE txz01 EQ @lt_nfe-txz01
              AND   loekz NE @gc_l
              .
          IF sy-subrc IS INITIAL.
            DATA(lt_ekpo) = gt_ekpo.
            SORT lt_ekpo BY ebeln.
            DELETE ADJACENT DUPLICATES FROM lt_ekpo COMPARING ebeln.
            IF lt_ekpo IS NOT INITIAL.
              SELECT ebeln, zterm
                FROM ekko
                INTO TABLE @gt_ekko
                FOR ALL ENTRIES IN @lt_ekpo
                WHERE ebeln EQ @lt_ekpo-ebeln
                .
              IF sy-subrc IS INITIAL.
                SORT gt_ekko BY ebeln.
              ENDIF.
            ENDIF.
            SORT gt_ekpo BY txz01.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.

      LOOP AT gt_nfe_entrada ASSIGNING FIELD-SYMBOL(<fs_nfe_entrada>).
        ls_nfe-txz01 = <fs_nfe_entrada>-doc_frete.
        APPEND ls_nfe TO lt_nfe.
        CLEAR  ls_nfe.
      ENDLOOP.

      SORT lt_nfe BY txz01.
      DELETE ADJACENT DUPLICATES FROM lt_nfe COMPARING txz01.

      IF lt_nfe[] IS NOT  INITIAL.

        SELECT ebeln, loekz, txz01, mwskz
          FROM ekpo
          INTO TABLE @gt_ekpo
          FOR ALL ENTRIES IN @lt_nfe
          WHERE txz01 EQ @lt_nfe-txz01
          AND   loekz NE @gc_l
          .
        IF sy-subrc IS INITIAL.
          DATA(lt_ekpo_aux) = gt_ekpo.
          SORT lt_ekpo_aux BY ebeln.
          DELETE ADJACENT DUPLICATES FROM lt_ekpo_aux COMPARING ebeln.
          IF lt_ekpo_aux IS NOT INITIAL.
            SELECT ebeln, zterm
              FROM ekko
              INTO TABLE @gt_ekko
              FOR ALL ENTRIES IN @lt_ekpo_aux
              WHERE ebeln EQ @lt_ekpo_aux-ebeln
              .
            IF sy-subrc IS INITIAL.
              SORT gt_ekko BY ebeln.
            ENDIF.
          ENDIF.
          SORT gt_ekpo BY txz01.
        ENDIF.
      ENDIF.
      rv_cenario = gc_cenario-transferencia.
    ENDIF.

  ENDMETHOD.


  METHOD cenario_transf.

    TYPES: BEGIN OF ty_lin_mkpf,
             mblnr TYPE mkpf-mblnr,
             mjahr TYPE mkpf-mjahr,
           END OF ty_lin_mkpf,
           BEGIN OF ty_lin_rseg,
             belnr TYPE rseg-belnr,
             gjahr TYPE rseg-gjahr,
           END OF ty_lin_rseg.

    CONSTANTS: lc_typee   TYPE c VALUE 'E',
               lc_types   TYPE c VALUE 'S',
               lc_id      TYPE c LENGTH 16 VALUE 'ZTM_EDOC_BR_FLEX',
               lc_number  TYPE c LENGTH 3 VALUE '000',
               lc_number5 TYPE c LENGTH 3 VALUE '005'.

    DATA: lv_mblnr    TYPE mkpf-mblnr,
          lv_mjahr    TYPE mkpf-mjahr,
          lv_belnr    TYPE rseg-belnr,
          lv_gjahr    TYPE rseg-gjahr,
          lt_lin_rseg TYPE TABLE OF ty_lin_rseg,
          ls_lin_mkpf TYPE ty_lin_mkpf,
          lt_lin_mkpf TYPE TABLE OF ty_lin_mkpf.

    DATA(lv_docnum) = retorno_docnum( io_cte ).

    IF lv_docnum IS NOT INITIAL.

      SELECT SINGLE docnum,
                     cancel,
                     direct
                FROM j_1bnfdoc
                INTO @DATA(ls_doc)
               WHERE docnum EQ @lv_docnum
                 AND cancel EQ @space
*                 AND direct EQ @gc_1
        .
      IF sy-subrc IS INITIAL.

        SELECT docnum,
               itmnum,
               reftyp,
               refkey
          FROM j_1bnflin
          INTO TABLE @DATA(lt_lin)
         WHERE docnum EQ @lv_docnum
*           AND itmnum EQ @gc_10
           AND ( itmnum EQ @gc_10 OR
                 itmnum EQ @gc_1 )
           AND reftyp EQ @gc_li
   .
        IF sy-subrc IS NOT INITIAL.

          SELECT docnum,
                 itmnum,
                 reftyp,
                 refkey
            FROM j_1bnflin
            INTO TABLE @lt_lin
           WHERE docnum EQ @lv_docnum
*           AND itmnum EQ @gc_10
           AND ( itmnum EQ @gc_10 OR
                 itmnum EQ @gc_1 )
             AND reftyp EQ @gc_md
     .
          IF sy-subrc IS INITIAL.

            SORT lt_lin BY refkey.
            DELETE ADJACENT DUPLICATES FROM lt_lin COMPARING refkey.

            lt_lin_mkpf = VALUE #( FOR ls_lin IN lt_lin (
                                       mblnr = ls_lin-refkey(10)
                                       mjahr = ls_lin-refkey+10(4) ) ).

            IF lt_lin_mkpf[] IS NOT INITIAL.

              SELECT mblnr,
                     mjahr,
                     le_vbeln
                FROM mkpf
          INTO TABLE @DATA(lt_mkpf)
             FOR ALL ENTRIES IN @lt_lin_mkpf
               WHERE mblnr EQ @lt_lin_mkpf-mblnr
                 AND mjahr EQ @lt_lin_mkpf-mjahr
         .
              IF sy-subrc IS INITIAL.
                SORT lt_mkpf BY le_vbeln.
                DELETE ADJACENT DUPLICATES FROM lt_mkpf COMPARING le_vbeln.
              ENDIF.
            ENDIF.

          ELSE.
            "Não é possível encontrar NFe
*            rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '000') ).
            rt_bapiret = VALUE #( BASE rt_bapiret ( type = lc_typee id = lc_id number = lc_number ) ).
          ENDIF.

        ELSE.

          SORT lt_lin BY refkey.
          DELETE ADJACENT DUPLICATES FROM lt_lin COMPARING refkey.

          lt_lin_rseg = VALUE #( FOR ls_lin_rseg IN lt_lin (
                                     belnr = ls_lin_rseg-refkey(10)
                                     gjahr = ls_lin_rseg-refkey+10(4) ) ).

          IF lt_lin_rseg[] IS NOT INITIAL.

            SELECT belnr,
                   gjahr,
                   buzei,
                   lfbnr,
                   lfgja
              FROM rseg
        INTO TABLE @DATA(lt_rseg)
           FOR ALL ENTRIES IN @lt_lin_rseg
             WHERE belnr EQ @lt_lin_rseg-belnr
               AND gjahr EQ @lt_lin_rseg-gjahr
       .
            IF sy-subrc IS INITIAL.

              SORT lt_rseg BY lfbnr lfgja.
              DELETE ADJACENT DUPLICATES FROM lt_rseg COMPARING lfbnr lfgja.

              lt_lin_mkpf = VALUE #( FOR ls_rseg IN lt_rseg (
                                         mblnr = ls_rseg-lfbnr
                                         mjahr = ls_rseg-lfgja ) ).

              IF lt_lin_mkpf[] IS NOT INITIAL.
*
                SELECT mblnr,
                       mjahr,
                       le_vbeln
                  FROM mkpf
            INTO TABLE @lt_mkpf
               FOR ALL ENTRIES IN @lt_lin_mkpf
                 WHERE mblnr EQ @lt_lin_mkpf-mblnr
                   AND mjahr EQ @lt_lin_mkpf-mjahr
.
                IF sy-subrc IS INITIAL.
                  SORT lt_mkpf BY le_vbeln.
                  DELETE ADJACENT DUPLICATES FROM lt_mkpf COMPARING le_vbeln.
                ENDIF.
                .
              ENDIF.
            ENDIF.
          ELSE.
            "Não é possível encontrar NFe
*            rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '000') ).
            rt_bapiret = VALUE #( BASE rt_bapiret ( type = lc_typee id = lc_id number = lc_number ) ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lt_mkpf[] IS NOT INITIAL.

      rt_bapiret = busca_frete( EXPORTING it_mkpf = lt_mkpf ).

      CHECK gt_frete[] IS NOT INITIAL.
      preenche_nfe_entrada( ).
      rt_bapiret =  grava_nfe_entrada( ).
      "Vínculo de CTe x Ord Frete OK
*      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'S' id = 'ZTM_EDOC_BR_FLEX' number = '005') ).
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = lc_types id = lc_id number = lc_number5 ) ).
    ELSE.
      "Não é possível encontrar NFe
*      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '000') ).
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = lc_typee id = lc_id number = lc_number ) ).
    ENDIF.

  ENDMETHOD.


  METHOD cenario_venda.

    TYPES: BEGIN OF ty_lin_vbrp,
             vbeln TYPE vbrp-vbeln,
             posnr TYPE vbrp-posnr,
           END OF ty_lin_vbrp.

    DATA: lt_lin_vbrp TYPE TABLE OF ty_lin_vbrp.

    DATA(lv_docnum) = retorno_docnum( io_cte ).

    IF lv_docnum IS NOT INITIAL.

      SELECT SINGLE docnum,
                    cancel,
                    direct
               FROM j_1bnfdoc
               INTO @DATA(ls_doc)
              WHERE docnum EQ @lv_docnum
                AND cancel EQ @space
                AND direct EQ @gc_2
        .
      IF sy-subrc IS INITIAL.

        SELECT docnum,
               itmnum,
               reftyp,
               refkey,
               refitm
          FROM j_1bnflin
    INTO TABLE @DATA(lt_lin)
         WHERE docnum EQ @lv_docnum
           AND ( itmnum EQ @gc_10 OR
                 itmnum EQ @gc_1 )
           AND reftyp EQ @gc_bi
   .
        IF sy-subrc IS INITIAL.

          SORT lt_lin BY refkey refitm.
          DELETE ADJACENT DUPLICATES FROM lt_lin COMPARING refkey refitm.

          lt_lin_vbrp = VALUE #( FOR ls_lin IN lt_lin (
                                     vbeln = ls_lin-refkey
                                     posnr = ls_lin-refitm ) ).

          IF lt_lin_vbrp[] IS NOT INITIAL.

            SELECT vbeln,
                   posnr,
                   vgbel
              FROM vbrp
           FOR ALL ENTRIES IN @lt_lin_vbrp
             WHERE vbeln EQ @lt_lin_vbrp-vbeln
               AND posnr EQ @lt_lin_vbrp-posnr
                      INTO TABLE @DATA(lt_vbrp)
.
          ENDIF.
        ELSE.
          "Não é possível encontrar NFe
          rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '000') ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF lt_vbrp[] IS NOT INITIAL.

      rt_bapiret = busca_frete( EXPORTING it_vbrp = lt_vbrp ).

      CHECK gt_frete[] IS NOT INITIAL.

      preenche_nfe_saida( ).
      rt_bapiret =  grava_nfe_saida( ).
      "Vínculo de CTe x Ord Frete OK
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'S' id = 'ZTM_EDOC_BR_FLEX' number = '005') ).
    ELSE.
      "Não é possível encontrar NFe
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '000') ).
    ENDIF.


  ENDMETHOD.


  METHOD define_cenario.

* Lógica para definir o processo REMESSA, TRANSFERÊNCIA OU VENDA

    DATA(ls_cte) = io_cte->retrieve_infcte( ).
    IF ls_cte-rem-cnpj IS NOT INITIAL.

      SELECT SINGLE bukrs,
                    stcd1
        FROM j_1bbranch
        INTO @DATA(ls_1bbranch)
        WHERE stcd1 EQ @ls_cte-rem-cnpj.

      IF sy-subrc IS INITIAL.
        IF ls_cte-dest-cnpj IS NOT INITIAL.
          SELECT SINGLE bukrs,
                        stcd1
            FROM j_1bbranch
            INTO @DATA(ls_1bbranch_aux)
            WHERE stcd1 EQ @ls_cte-dest-cnpj.
          IF sy-subrc IS INITIAL.
            rv_cenario = gc_cenario-transferencia.
            gv_bukrs   = ls_1bbranch_aux-bukrs.
          ELSE.
            rv_cenario = gc_cenario-venda.
            gv_bukrs   = ls_1bbranch-bukrs.
          ENDIF.
        ENDIF.
      ELSE.
        IF ls_cte-dest-cnpj IS NOT INITIAL.
          SELECT SINGLE bukrs,
                        stcd1
            FROM j_1bbranch
            INTO @DATA(ls_1bbranch_aux1)
            WHERE stcd1 EQ @ls_cte-dest-cnpj.
          IF sy-subrc IS INITIAL.
            rv_cenario = gc_cenario-transferencia.
            gv_bukrs   = ls_1bbranch_aux1-bukrs.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD executa_bapi.

    DATA: lt_return      TYPE STANDARD TABLE OF bapiret2,
          lt_item_data   TYPE TABLE OF  bapi_incinv_create_item,
          ls_export_flex TYPE zstm_export_flex.

    CONSTANTS: lc_tr      TYPE c VALUE '-',
               lc_po      TYPE c VALUE ':',
               lc_sp      TYPE c VALUE ' ',
               lc_typee   TYPE c VALUE 'E',
               lc_types   TYPE c VALUE 'S',
               lc_id      TYPE c LENGTH 16 VALUE 'ZTM_EDOC_BR_FLEX',
               lc_number3 TYPE c LENGTH 3 VALUE '003',
               lc_number4 TYPE c LENGTH 3 VALUE '004'.

    DATA(lv_chave_cte) = io_cte->retrieve_infprot( )-chcte.
    gv_chave_cte = lv_chave_cte.
    gv_versao   = io_cte->retrieve_version( ).
    gv_dhrecbto = io_cte->retrieve_infprot( )-dhrecbto.
    gv_dhemi = io_cte->retrieve_infcte( )-ide-dhemi.

    REPLACE ALL OCCURRENCES OF lc_tr IN gv_dhrecbto WITH lc_sp.
    REPLACE ALL OCCURRENCES OF lc_po IN gv_dhrecbto WITH lc_sp.
    REPLACE ALL OCCURRENCES OF lc_tr IN gv_dhemi WITH lc_sp.
    REPLACE ALL OCCURRENCES OF lc_po IN gv_dhemi WITH lc_sp.

    ls_export_flex-authcod  = gv_chave_cte+34(10).
    ls_export_flex-authdate = gv_dhrecbto(8).
    ls_export_flex-authtime = gv_dhrecbto+9(6).
    ls_export_flex-hemi     = gv_dhemi+9(6).
    ls_export_flex-xmlvers  = gv_versao.
    ls_export_flex-docnum9  = gv_chave_cte+34(09).


    GET TIME STAMP FIELD DATA(lv_ts).

    DATA(lv_task) = |{ lv_ts }|.

    CALL FUNCTION 'ZFMTM_EDOC_BR_FLEX_V1'
      EXPORTING
        is_headerdata       = gs_headerdata
        is_ctedoc           = gs_ctedoc
      TABLES
        tt_cteref           = gt_cteref
        tt_ctestx           = gt_ctestx
        tt_ctelin           = gt_ctelin
        tt_return           = gt_return
      CHANGING
        cv_invoicedocnumber = gv_invoicedocnumber
        cv_fiscalyear       = gv_fiscalyear.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.

    IF gv_invoicedocnumber IS NOT INITIAL.
      MOVE-CORRESPONDING lt_return TO rt_bapiret.
      "Documento &1/&2 (doc/ano) gerado com sucesso
*      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'S' id = 'ZTM_EDOC_BR_FLEX' number = '003'
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = lc_types id = lc_id number = lc_number3
                                              message_v1 = gv_invoicedocnumber  message_v2 = gv_fiscalyear ) ).
    ELSE.
      "Não foi possível lançar a MIRO
*      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '004' ) ).
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = lc_typee id = lc_id number = lc_number4 ) ).
    ENDIF.

  ENDMETHOD.


  METHOD grava_nfe_entrada.

    MODIFY zttm_nfe_entrada FROM TABLE gt_nfe_entrada.

    IF sy-subrc IS NOT INITIAL.
      "Erro ao salva na tabela ZTTM_NFE_ENTRADA
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '002' message_v1 = 'ZTTM_NFE_ENTRADA' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD grava_nfe_saida.

    MODIFY zttm_nfe_saida FROM TABLE gt_nfe_saida.

    IF sy-subrc IS NOT INITIAL.
      "Erro ao salva na tabela ZTTM_NFE_SAIDA
      rt_bapiret = VALUE #( BASE rt_bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '002' message_v1 = 'ZTTM_NFE_SAIDA' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD if_edoc_br_flex~step_after_dacte ##NEEDED.

    bapiret = preenche_bapi( io_cte ).

  ENDMETHOD.


  METHOD if_edoc_br_flex~step_after_dacteos ##NEEDED.
  ENDMETHOD.


  METHOD if_edoc_br_flex~step_before_dacte.

    DATA(lv_chave_cte) = io_cte->retrieve_infprot( )-chcte.
    gv_chave_cte = lv_chave_cte.

    DATA(lv_cenario) = define_cenario( io_cte = io_cte ).

    IF gv_bukrs IS INITIAL.
      "Parceiro de negócio não possui CNPJ da SIM
      bapiret = VALUE #( BASE bapiret ( type = 'E' id = 'ZTM_EDOC_BR_FLEX' number = '006') ).
      RETURN.
    ENDIF.

    CASE lv_cenario.

      WHEN gc_cenario-transferencia.

        bapiret = cenario_transf( io_cte = io_cte ).

      WHEN gc_cenario-venda.

        bapiret = cenario_venda( io_cte = io_cte ).

    ENDCASE.

  ENDMETHOD.


  METHOD if_edoc_br_flex~step_before_dacteos ##NEEDED.
  ENDMETHOD.


  METHOD preenche_bapi.

    DATA: lv_doc_ref TYPE xblnr.

    DATA(lv_chave_cte) = io_cte->retrieve_infprot( )-chcte.
    gv_chave_cte = lv_chave_cte.
    gv_vtprest   = io_cte->retrieve_vprest( )-vtprest.
    DATA(lv_nct) = io_cte->retrieve_infcte( )-ide-nct.
    DATA(lv_serie) = io_cte->retrieve_infcte( )-ide-serie.

    lv_doc_ref = |{ lv_nct }| & |-| & |{ lv_serie }|.

    gv_dhemi = io_cte->retrieve_infcte( )-ide-dhemi.

    REPLACE ALL OCCURRENCES OF '-' IN gv_dhrecbto WITH ' '.
    REPLACE ALL OCCURRENCES OF ':' IN gv_dhrecbto WITH ' '.
    REPLACE ALL OCCURRENCES OF '-' IN gv_dhemi WITH ' '.
    REPLACE ALL OCCURRENCES OF ':' IN gv_dhemi WITH ' '.


    DATA(lv_cenario) = busca_tabela( EXPORTING iv_chave_cte = gv_chave_cte ).

    DATA(lv_cont) = 0.

    CASE lv_cenario.
      WHEN gc_cenario-transferencia.

        DATA(lt_tm_itemdata) = VALUE ty_tm_item( FOR ls_nfe_entrada IN gt_nfe_entrada
                                                 FOR ls_ekpo IN gt_ekpo WHERE ( txz01 = ls_nfe_entrada-doc_frete )
                                                 (
                                                     invoice_doc_item = lv_cont + 1
                                                     tor_number       = |{ ls_nfe_entrada-doc_frete ALPHA = IN }|
                                                     amt_doccur       = gv_vtprest
                                                     tax_code         = ls_ekpo-mwskz ) ).

        MOVE-CORRESPONDING lt_tm_itemdata TO gt_tm_itemdata.

        IF gt_nfe_entrada[] IS NOT INITIAL.
          IF gv_bukrs IS INITIAL.
            gv_bukrs = gt_nfe_entrada[ 1 ]-bukrs.
          ENDIF.
        ENDIF.
        IF gt_ekko[] IS NOT INITIAL.
          gv_dzterm = gt_ekko[ 1 ]-zterm.
        ENDIF.
        gs_headerdata = VALUE #(
                 interfvers   = '001'
                 direct_goods = '1'
                 cnpj_servtaker = io_cte->retrieve_dest( )-cnpj
*                                 gross_amount = gv_vtprest
                                 ).

      WHEN gc_cenario-venda.

        gt_tm_itemdata = VALUE #( FOR ls_nfe_saida IN gt_nfe_saida
                                  FOR ls_ekpo2 IN gt_ekpo WHERE ( txz01 = ls_nfe_saida-doc_frete )
                                  (
                                      invoice_doc_item = lv_cont + 1
                                      tor_number       = |{ ls_nfe_saida-doc_frete ALPHA = IN }|
                                      amt_doccur       = gv_vtprest
                                      tax_code         = ls_ekpo2-mwskz ) ).


        IF gt_nfe_saida[] IS NOT INITIAL.
          IF gv_bukrs IS INITIAL.
            gv_bukrs = gt_nfe_saida[ 1 ]-bukrs.
          ENDIF.
        ENDIF.
        IF gt_ekko[] IS NOT INITIAL.
          gv_dzterm = gt_ekko[ 1 ]-zterm.
        ENDIF.


        gs_headerdata = VALUE #(
                 interfvers   = '001'
                 direct_goods = '1'
                 cnpj_servtaker = io_cte->retrieve_dest( )-cnpj
*                                 gross_amount = gv_vtprest
                                 ).

    ENDCASE.


    DATA(lv_versao)   = io_cte->retrieve_version( ).
    DATA(lv_dhrecbto) = io_cte->retrieve_infprot( )-dhrecbto.
    DATA(lv_chave_cte_aux) = io_cte->retrieve_infprot( )-chcte.
    DATA(lv_chave_len) = strlen( lv_chave_cte ).

    gs_ctedoc-xmlvers = '3,00'.
    gs_ctedoc-budat   = sy-datum.
    gs_ctedoc-bldat   = lv_dhrecbto(8).
    gs_ctedoc-nfnet   = gv_vtprest.
    gs_ctedoc-nftot   = gv_vtprest.

    gs_ctedoc-access_key-regio = lv_chave_cte(2).
    gs_ctedoc-access_key-nfyear = lv_chave_cte+2(2).
    gs_ctedoc-access_key-nfmonth = lv_chave_cte+4(2).
    gs_ctedoc-access_key-stcd1 = io_cte->retrieve_dest(  )-cnpj.
    gs_ctedoc-access_key-model = '57'.
    gs_ctedoc-access_key-serie = lv_serie.
    gs_ctedoc-access_key-nfnum9 = lv_nct.
    gs_ctedoc-access_key-docnum9 = lv_chave_cte+34(09).

    lv_chave_len = lv_chave_len - 1.
    gs_ctedoc-access_key-cdv = lv_chave_cte+lv_chave_len.

    gs_ctedoc-authcode  = lv_chave_cte+34(10).
    gs_ctedoc-authdate  = lv_dhrecbto(8).
    gs_ctedoc-authtime  = gv_dhrecbto+9(6).

    gs_ctedoc-serv_tp  = io_cte->retrieve_ide(  )-tpserv.
    gs_ctedoc-regio_fr  = io_cte->retrieve_ide(  )-ufini.
    gs_ctedoc-regio_to = io_cte->retrieve_ide(  )-uffim.
    gs_ctedoc-modetransport = io_cte->retrieve_ide(  )-modal.
    gs_ctedoc-receiver_collect = io_cte->retrieve_ide(  )-retira.

    DATA ls_ctelin TYPE j_1bcte_lin.
    DATA ls_ctestx TYPE j_1bcte_stx.

    IF gs_ctedoc-regio_fr = gs_ctedoc-regio_to.
      ls_ctelin-cfop = '1353/AA'.
    ELSE.
      ls_ctelin-cfop  = '2353/AA'.
    ENDIF.

    ls_ctelin-mwskz = gt_tm_itemdata[ 1 ]-tax_code.

    APPEND ls_ctelin TO gt_ctelin.
    DATA lv_num_aux TYPE ty_dec2.
    lv_num_aux = ( ls_ctestx-taxval / ls_ctestx-base * 100 ).

    "ls_ctelin-Tabela = GT_CTESTX "(1 linha).
    ls_ctestx-taxgrp = 'ICMS'.
    ls_ctestx-base   = gv_vtprest.
    ls_ctestx-rate   = lv_num_aux.
    ls_ctestx-taxval = io_cte->retrieve_infcte( )-imp-vtottrib.

    APPEND ls_ctestx to gt_ctestx.

    DATA ls_cteref TYPE j_1bcte_ref.

    DATA(lt_infnfe) = io_cte->retrieve_infcte( )-infctenorm-infdoc-infnfe.

    IF lt_tm_itemdata IS NOT INITIAL.
      SELECT /scmtms/d_torrot~db_key,
             tor_id,
             /scmtms/d_torite~base_btd_id
      FROM /scmtms/d_torrot
      INNER JOIN /scmtms/d_torite on parent_key = /scmtms/d_torrot~db_key
      INTO @DATA(LV_DELIVERY_REF_OF)
      UP TO 1 ROWS
      FOR ALL ENTRIES IN @lt_tm_itemdata
      WHERE tor_id = @lt_tm_itemdata-tor_number.
      ENDSELECT.
    ENDIF.

    LOOP AT lt_infnfe ASSIGNING FIELD-SYMBOL(<fs_infnfe>).
      ls_cteref-regio  = <fs_infnfe>-chave(2). "dois primeiros dígitos                        43
      ls_cteref-nfyear  = <fs_infnfe>-chave+2(2). "terceiro e quarto dígitos                       24
      ls_cteref-nfmonth  = <fs_infnfe>-chave+4(2). "quinto e sexto dígitos                      01
      ls_cteref-stcd1 = <fs_infnfe>-chave+6(14). "14 próximos dígitos (sétimo ao vigésimo)            42150391003862
      ls_cteref-model =   <fs_infnfe>-chave+20(2). "vigésimo primeiro e vigésimo segundo dígitos                       55
      ls_cteref-serie  = <fs_infnfe>-chave+22(3). "vigésimo terceiro ao vigésimo quinto                        001
      ls_cteref-nfnum9  = <fs_infnfe>-chave+25(9). "vigésimo sexto ao trigésimo quarto                     000445357
      ls_cteref-docnum9  = <fs_infnfe>-chave+34(9). "trigésimo quinto ao quadragésimo terceiro                     176758530
      ls_cteref-cdv  = <fs_infnfe>-chave+43(1). "último dígito                          9
      ls_cteref-vbeln = lv_delivery_ref_of-base_btd_id.
      APPEND ls_cteref TO gt_cteref.
    ENDLOOP.


    rt_bapiret =   executa_bapi( io_cte ).
    IF gv_invoicedocnumber IS NOT INITIAL.

      rt_bapiret = atualiza_tabela( lv_cenario ).

    ENDIF.

  ENDMETHOD.


  METHOD preenche_nfe_entrada.

    FREE: gt_nfe_entrada[].

    gt_nfe_entrada = VALUE #( FOR ls_frete IN gt_frete
                              ( mandt     = sy-mandt
                                chave_nfe = gv_chave_nfe
                                doc_frete = ls_frete-doc_frete
                                chave_cte = gv_chave_cte
                                bukrs     = gv_bukrs
                              ) ).

  ENDMETHOD.


  METHOD preenche_nfe_saida.

    FREE: gt_nfe_saida[].

    gt_nfe_saida = VALUE #( FOR ls_frete IN gt_frete
                              ( mandt     = sy-mandt
                                chave_nfe = gv_chave_nfe
                                doc_frete = ls_frete-doc_frete
                                chave_cte = gv_chave_cte
                                bukrs     = gv_bukrs
                              ) ).

  ENDMETHOD.


  METHOD retorno_docnum.

    DATA: lv_regio   TYPE j_1bnfe_active-regio,
          lv_nfyear  TYPE j_1bnfe_active-nfyear,
          lv_nfmonth TYPE j_1bnfe_active-nfmonth,
          lv_stcd1   TYPE j_1bnfe_active-stcd1,
          lv_model   TYPE j_1bnfe_active-model,
          lv_serie   TYPE j_1bnfe_active-serie,
          lv_nfnum9  TYPE j_1bnfe_active-nfnum9,
          lv_docnum9 TYPE j_1bnfe_active-docnum9,
          lv_cdv     TYPE j_1bnfe_active-cdv.

    DATA(lt_cte) = io_cte->retrieve_infcte( ).

    IF lt_cte-infctenorm-infdoc-infnfe[]  IS NOT INITIAL.

      DATA(ls_cte)     = lt_cte-infctenorm-infdoc-infnfe[ 1 ] .

      gv_chave_nfe = ls_cte-chave.

      IF sy-subrc IS INITIAL.
        lv_regio   = ls_cte-chave(2).
        lv_nfyear  = ls_cte-chave+2(2).
        lv_nfmonth = ls_cte-chave+4(2).
        lv_stcd1   = ls_cte-chave+6(14).
        lv_model   = ls_cte-chave+20(2).
        lv_serie   = ls_cte-chave+22(3).
        lv_nfnum9  = ls_cte-chave+25(9).
        lv_docnum9 = ls_cte-chave+34(9).
        lv_cdv     = ls_cte-chave+43(1).

        SELECT SINGLE docnum,
                      regio,
                      nfyear,
                      nfmonth,
                      stcd1,
                      model,
                      serie,
                      nfnum9,
                      docnum9,
                      cdv
                 FROM j_1bnfe_active
                 INTO @DATA(ls_active)
                WHERE regio    EQ @lv_regio
                  AND nfyear   EQ @lv_nfyear
                  AND nfmonth  EQ @lv_nfmonth
                  AND stcd1    EQ @lv_stcd1
                  AND model    EQ @lv_model
                  AND serie    EQ @lv_serie
                  AND nfnum9   EQ @lv_nfnum9
                  AND docnum9  EQ @lv_docnum9
                  AND cdv      EQ @lv_cdv
                  AND cancel   EQ @space
                  AND direct   EQ @gc_2.
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE docnum,
                        regio,
                        nfyear,
                        nfmonth,
                        stcd1,
                        model,
                        serie,
                        nfnum9,
                        docnum9,
                        cdv
                   FROM j_1bnfe_active
                   INTO @ls_active
                  WHERE regio    EQ @lv_regio
                    AND nfyear   EQ @lv_nfyear
                    AND nfmonth  EQ @lv_nfmonth
                    AND stcd1    EQ @lv_stcd1
                    AND model    EQ @lv_model
                    AND serie    EQ @lv_serie
                    AND nfnum9   EQ @lv_nfnum9
                    AND docnum9  EQ @lv_docnum9
                    AND cdv      EQ @lv_cdv
                    AND cancel   EQ @space
                    AND direct   EQ @gc_1
            .
        ENDIF.
        .
*        IF sy-subrc IS INITIAL.
        IF ls_active IS NOT INITIAL.

          rv_docnum = ls_active-docnum.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD task_finish.

    DATA: lt_return    TYPE STANDARD TABLE OF bapiret2.

    RECEIVE RESULTS FROM FUNCTION 'ZFMTM_ALT_PROC_LOGISTICA'
      TABLES
        tt_return           = gt_return
        tt_tm_itemdata      = gt_tm_itemdata
      CHANGING
        cv_invoicedocnumber = gv_invoicedocnumber
        cv_fiscalyear       = gv_fiscalyear.

    gv_result = abap_true .

  ENDMETHOD.
ENDCLASS.
