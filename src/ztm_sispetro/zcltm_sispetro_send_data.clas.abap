CLASS zcltm_sispetro_send_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      " Compartimentação
      BEGIN OF ty_compartimentacao_oc,
        Compartimento TYPE string,
        CodProd       TYPE string,
        Qtde          TYPE string,
      END OF ty_compartimentacao_oc .
    TYPES:
                         " Tabela de Compartimentação
      ty_t_compartimentacao_oc TYPE TABLE OF ty_compartimentacao_oc WITH DEFAULT KEY .
    TYPES:
      " Ordem de Carregamento
      BEGIN OF ty_ordem_carregamento,
        CnpjEmpresa         TYPE string,
        Tipo                TYPE string,
        data                TYPE string,
        CodTransp           TYPE string,
        SeqFrota            TYPE string,
        SeqFrotaCarreta     TYPE string,
        SeqFrotaTrem        TYPE string,
        SeqFrota4           TYPE string,
        ColetaAmostra       TYPE string,
        webNumeroOCCliente  TYPE string,
        observacao          TYPE string,
        compartimentacao_oc TYPE ty_t_compartimentacao_oc,
      END OF ty_ordem_carregamento .
    TYPES:
                         " Tabela de Ordem de Carregamento
      ty_t_ordem_carregamento TYPE TABLE OF ty_ordem_carregamento WITH DEFAULT KEY .
    TYPES:
      " Cabeçalho
      BEGIN OF ty_header,
        ordem_carregamento TYPE ty_ordem_carregamento,
      END OF ty_header .
    TYPES:
                " Tabela de Cabeçalho
      ty_t_header TYPE TABLE OF ty_header WITH DEFAULT KEY .
    TYPES:
      " Cancelamento de OC
      BEGIN OF ty_cancel,
        cnpjempresa TYPE string,
        id          TYPE string,
      END OF ty_cancel .
    TYPES:
      " Cancelamento de OC
      BEGIN OF ty_num_oc,
        NumeroOC TYPE string,
      END OF ty_num_oc .

    TYPES:
      " Cancelamento de OC
      BEGIN OF ty_num_oc1,
        NumeroOC TYPE string,
      END OF ty_num_oc1.

    TYPES:
      " Cabeçalho
      BEGIN OF ty_errormessages,
        message TYPE string,
      END OF ty_errormessages .
    TYPES:
                " Tabela de Cabeçalho
      ty_t_errormessages TYPE TABLE OF string WITH DEFAULT KEY .

    TYPES:
      " Cabeçalho
      BEGIN OF ty_num_oc_data1,
        data          TYPE ty_num_oc,
        status        TYPE string,
        title         TYPE string,
        errorMessages TYPE ty_t_errormessages,
        stackTrace    TYPE string,
      END OF ty_num_oc_data1.



    CONSTANTS:
      " Constantes para integração CPI
      BEGIN OF gc_cpi,
        processo    TYPE ze_processo   VALUE 'ZTM_SISPETRO_SEND_DATA' ##NO_TEXT,
        method_post TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
      END OF gc_cpi .
    CONSTANTS:
      " Constantes para integração CPI - Cancelamento
      BEGIN OF gc_cpi_cancel,
        processo    TYPE ze_processo   VALUE 'ZTM_SISPETRO_SEND_DATA_CANCEL' ##NO_TEXT,
        method_post TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
      END OF gc_cpi_cancel .
    CONSTANTS:
      " Constantes para integração CPI
      BEGIN OF gc_message,
        id         TYPE arbgb   VALUE 'ZTM_SISPETRO' ##NO_TEXT,
        number_001 TYPE symsgno VALUE '001' ##NO_TEXT,
        number_002 TYPE symsgno VALUE '002' ##NO_TEXT,
        number_003 TYPE symsgno VALUE '003' ##NO_TEXT,
        number_004 TYPE symsgno VALUE '004' ##NO_TEXT,
      END OF gc_message .

    METHODS exec
      IMPORTING
        !it_key          TYPE /bobf/t_frw_key
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS finish_integracao
      IMPORTING
        !p_task TYPE any .
    METHODS cancel_oc
      IMPORTING
        !it_key          TYPE /bobf/t_frw_key
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS finish_integracao_cancel
      IMPORTING
        !p_task TYPE any .
    CLASS-METHODS update_tor_root
      IMPORTING
        !iv_tor_id TYPE /scmtms/tor_id
        !iv_num_oc TYPE ty_num_oc-numerooc .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_return_task TYPE bapiret2_t .

    METHODS get_data
      IMPORTING
        !it_key        TYPE /bobf/t_frw_key
      RETURNING
        VALUE(rt_data) TYPE ty_t_header .
ENDCLASS.



CLASS ZCLTM_SISPETRO_SEND_DATA IMPLEMENTATION.


  METHOD cancel_oc.

    DATA: ls_cancel TYPE zcltm_sispetro_send_data=>ty_cancel.

    " Busca TOR Data
    CALL METHOD /scmtms/cl_tor_helper_common=>get_tor_data
      EXPORTING
        it_root_key  = it_key
      IMPORTING
        et_root      = DATA(lt_tor_root)
        et_all_items = DATA(lt_tor_item).

    CHECK lt_tor_root IS NOT INITIAL AND
          lt_tor_item IS NOT INITIAL.

    SELECT werks, lifnr FROM t001w
      FOR ALL ENTRIES IN @lt_tor_item
     WHERE werks EQ @lt_tor_item-erp_plant_id
    INTO TABLE @DATA(lt_empresas).
    IF sy-subrc = 0.
      SORT lt_empresas BY lifnr.
      SELECT partner,
             taxnum
        FROM dfkkbptaxnum
        INTO TABLE @DATA(lt_taxnum_empresas)
        FOR ALL ENTRIES IN @lt_empresas
       WHERE partner EQ @lt_empresas-lifnr
         AND taxtype EQ 'BR1'.
      IF sy-subrc = 0.
        SORT lt_taxnum_empresas BY partner.
      ENDIF.
    ENDIF.

    LOOP AT lt_tor_root ASSIGNING FIELD-SYMBOL(<fs_tor_root>).
      IF <fs_tor_root>-zz_sispetro_num IS NOT INITIAL.

        IF line_exists( lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ). "#EC CI_SORTSEQ
          IF lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]-erp_plant_id IS NOT INITIAL. "#EC CI_SORTSEQ
            DATA(lv_plant_id) = lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]-erp_plant_id. "#EC CI_SORTSEQ
            READ TABLE lt_empresas ASSIGNING FIELD-SYMBOL(<fs_empresas>) WITH KEY werks = lv_plant_id BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_taxnum_empresas ASSIGNING FIELD-SYMBOL(<fs_taxnum_empresas>)
              WITH KEY partner = <fs_empresas>-lifnr BINARY SEARCH.
              IF sy-subrc = 0.
                ls_cancel-cnpjempresa =
                |{ <fs_taxnum_empresas>-taxnum(2) }.{ <fs_taxnum_empresas>-taxnum+2(3) }.{ <fs_taxnum_empresas>-taxnum+5(3) }/{ <fs_taxnum_empresas>-taxnum+8(4) }-{ <fs_taxnum_empresas>-taxnum+12(2) }|.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        ls_cancel-id = <fs_tor_root>-zz_sispetro_num.
        DATA(lv_cancel) = /ui2/cl_json=>serialize( ls_cancel ).

        CALL FUNCTION 'ZFMTM_SISPETRO_SEND_DATA_CANC'
          STARTING NEW TASK <fs_tor_root>-zz_sispetro_num
          CALLING me->finish_integracao_cancel ON END OF TASK
          EXPORTING
            iv_cancel = lv_cancel.

        WAIT UNTIL lines( me->gt_return_task ) > 0.

        APPEND LINES OF me->gt_return_task TO rt_return.
        CLEAR: me->gt_return_task.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD exec.
    CONSTANTS: lc_e  TYPE c LENGTH 1 VALUE 'E',
               lc_no TYPE symsgno VALUE '005',
               lc_id TYPE symsgid VALUE 'ZTM_SISPETRO'.
    CHECK it_key IS NOT INITIAL.

    DATA(lt_header) = get_data( it_key = it_key ).      "#EC CI_CONV_OK

    CHECK lt_header IS NOT INITIAL.

    CALL METHOD /scmtms/cl_tor_helper_common=>get_tor_data
      EXPORTING
        it_root_key = it_key
      IMPORTING
        et_root     = DATA(lt_tor_root).

    CHECK lt_tor_root IS NOT INITIAL.

    SELECT _log~id_processo, MAX( _log~id_msgs ) AS id_msgs
    FROM zttm_logsispetr0 AS _log
    INNER JOIN @lt_tor_root AS _torrot
    ON _log~id_processo = _torrot~key
    GROUP BY _log~id_processo
    INTO TABLE @DATA(lt_last_log).

    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
      DATA lv_erro TYPE string.
      IF <fs_header>-ordem_carregamento-cnpjempresa IS INITIAL.
        IF lv_erro IS NOT INITIAL.
          lv_erro = |{ lv_erro }, { TEXT-t01 }|.
        ELSE.
          lv_erro = |{ TEXT-t01 }|.
        ENDIF.
        IF <fs_header>-ordem_carregamento-tipo IS INITIAL.
          IF lv_erro IS NOT INITIAL.
            lv_erro = |{ lv_erro }, { TEXT-t02 }|.
          ELSE.
            lv_erro = |{ TEXT-t02 }|.
          ENDIF.
        ENDIF.
        IF <fs_header>-ordem_carregamento-Data IS INITIAL.
          IF lv_erro IS NOT INITIAL.
            lv_erro = |{ lv_erro }, { TEXT-t03 }|.
          ELSE.
            lv_erro = |{ TEXT-t03 }|.
          ENDIF.
        ENDIF.
        IF <fs_header>-ordem_carregamento-codtransp IS INITIAL.
          IF lv_erro IS NOT INITIAL.
            lv_erro = |{ lv_erro }, { TEXT-t04 }|.
          ELSE.
            lv_erro = |{ TEXT-t04 }|.
          ENDIF.
        ENDIF.
        IF <fs_header>-ordem_carregamento-seqfrota IS INITIAL.
          IF lv_erro IS NOT INITIAL.
            lv_erro = |{ lv_erro }, { TEXT-t05 }|.
          ELSE.
            lv_erro = |{ TEXT-t05 }|.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lv_erro IS INITIAL.

        DATA(lv_header) = /ui2/cl_json=>serialize( <fs_header> ).

        CALL FUNCTION 'ZFMTM_SISPETRO_SEND_DATA'
          STARTING NEW TASK <fs_header>-ordem_carregamento-webnumerooccliente
          CALLING me->finish_integracao ON END OF TASK
          EXPORTING
            iv_header = lv_header.

        WAIT UNTIL lines( me->gt_return_task ) > 0.

      ELSE.

        DATA(ls_return) = VALUE bapiret2( type = 'E'
                                          id = lc_id
                                          number = lc_no
                                          message_v1 = lv_erro ).
        APPEND ls_return TO gt_return_task.

      ENDIF.

      APPEND LINES OF me->gt_return_task TO rt_return.

      DATA(lv_tor_id) = CONV /scmtms/s_tor_root_k-tor_id( |{ <fs_header>-ordem_carregamento-webnumerooccliente ALPHA = IN }| ).
      READ TABLE lt_tor_root ASSIGNING FIELD-SYMBOL(<fs_tor_root>) WITH TABLE KEY tor_id COMPONENTS tor_id = lv_tor_id.
      IF sy-subrc = 0.
        DATA(lv_id_msgs) = VALUE #( lt_last_log[ id_processo = <fs_tor_root>-key ]-id_msgs OPTIONAL ).
        LOOP AT me->gt_return_task ASSIGNING FIELD-SYMBOL(<fs_return>).
          DATA: lv_tsl TYPE timestampl.
          GET TIME STAMP FIELD lv_tsl.
          ADD 1 TO lv_id_msgs.
          IF <fs_return>-message IS INITIAL.
            MESSAGE ID <fs_return>-id
            TYPE   'I'
            NUMBER <fs_return>-number
            WITH   <fs_return>-message_v1
                   <fs_return>-message_v2
                   <fs_return>-message_v3
                   <fs_return>-message_v4
            INTO   DATA(lv_string_msg).
            <fs_return>-message = lv_string_msg.
          ENDIF.

          INSERT zttm_logsispetr0 FROM @( VALUE #(  "#EC CI_IMUD_NESTED
            id_processo = <fs_tor_root>-key
            id_msgs = lv_id_msgs
            processo = COND #( WHEN <fs_return>-type = 'S' THEN <fs_return>-message_v2 )
            type    = <fs_return>-type
            status  = <fs_return>-type
            id      = <fs_return>-id
            numero  = <fs_return>-number
            message = <fs_return>-message
            created_by = sy-uname
            created_at = lv_tsl
            last_changed_by = sy-uname
            last_changed_at = lv_tsl
            local_last_changed_at = lv_tsl
          ) ).
        ENDLOOP.
      ENDIF.

      CLEAR: me->gt_return_task.
    ENDLOOP.

  ENDMETHOD.


  METHOD finish_integracao.
    DATA lv_error_cpi TYPE string.

    RECEIVE RESULTS FROM FUNCTION 'ZFMTM_SISPETRO_SEND_DATA'
      IMPORTING
        et_return = me->gt_return_task
        ev_return = lv_error_cpi.
*
*    IF line_exists( me->gt_return_task[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
*      APPEND VALUE #( inventorydocument = p_task type = if_xo_const_message~error error = lv_error_cpi ) TO me->gt_ref_error.
*    ENDIF.

    RETURN.
  ENDMETHOD.


  METHOD finish_integracao_cancel.
    DATA lv_error_cpi TYPE string.

    RECEIVE RESULTS FROM FUNCTION 'ZFMTM_SISPETRO_SEND_DATA_CANC'
      IMPORTING
        et_return = me->gt_return_task
        ev_return = lv_error_cpi.
*
*    IF line_exists( me->gt_return_task[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
*      APPEND VALUE #( inventorydocument = p_task type = if_xo_const_message~error error = lv_error_cpi ) TO me->gt_ref_error.
*    ENDIF.

    RETURN.
  ENDMETHOD.


  METHOD get_data.
    CONSTANTS: lc_item_cat_prd    TYPE /scmtms/d_torite-item_cat      VALUE 'PRD',
               lc_item_cat_dri    TYPE /scmtms/d_torite-item_cat      VALUE 'DRI',
               lc_base_btd_tco_58 TYPE /scmtms/d_torite-base_btd_tco  VALUE '58',
               lc_base_btd_tco_73 TYPE /scmtms/d_torite-base_btd_tco  VALUE '73'.

    " Busca TOR Data
    CALL METHOD /scmtms/cl_tor_helper_common=>get_tor_data
      EXPORTING
        it_root_key  = it_key
      IMPORTING
        et_root      = DATA(lt_tor_root)
        et_all_items = DATA(lt_tor_item).

    CHECK lt_tor_root IS NOT INITIAL AND
          lt_tor_item IS NOT INITIAL.

    SELECT werks, lifnr FROM t001w
      FOR ALL ENTRIES IN @lt_tor_item
     WHERE werks EQ @lt_tor_item-erp_plant_id
    INTO TABLE @DATA(lt_empresas).
    IF sy-subrc = 0.
      SORT lt_empresas BY lifnr.
      SELECT partner,
             taxnum
        FROM dfkkbptaxnum
        INTO TABLE @DATA(lt_taxnum_empresas)
        FOR ALL ENTRIES IN @lt_empresas
       WHERE partner EQ @lt_empresas-lifnr
         AND taxtype EQ 'BR1'.
      IF sy-subrc = 0.
        SORT lt_taxnum_empresas BY partner.
      ENDIF.
    ENDIF.


*    SELECT branch,
*           businessplacestatetaxnumber
*      FROM i_br_businessplace
*      INTO TABLE @DATA(lt_businessplace)
*      FOR ALL ENTRIES IN @lt_tor_item
*     WHERE branch EQ @lt_tor_item-erp_plant_id.
*
*    CHECK lt_businessplace IS NOT INITIAL.

*    SELECT partner,
*           taxnum
*      FROM dfkkbptaxnum
*      INTO TABLE @DATA(lt_taxnum)
*      FOR ALL ENTRIES IN @lt_tor_root
*     WHERE partner EQ @lt_tor_root-tspid
*       AND taxtype EQ 'BR1'.
*
*    CHECK lt_taxnum IS NOT INITIAL.

    DATA(lt_tor_item_dri) = VALUE /scmtms/t_tor_item_tr_k( FOR ls_tor_item IN lt_tor_item WHERE ( item_cat = lc_item_cat_dri ) ( ls_tor_item ) ).
    IF lt_tor_item_dri IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM lt_tor_item_dri COMPARING res_id.
      SELECT partner,
            taxnum
       FROM dfkkbptaxnum
       INTO TABLE @DATA(lt_taxnum)
       FOR ALL ENTRIES IN @lt_tor_item_dri
      WHERE partner EQ @lt_tor_item_dri-res_id(10)
        AND taxtype EQ 'BR2'.
    ENDIF.




    SELECT  db_key,
            parent_key,
            zz_sample_envelope
      FROM /scmtms/d_torite
     FOR ALL ENTRIES IN @lt_tor_root
     WHERE parent_key         =  @lt_tor_root-key
       AND zz_sample_envelope <> @space
     INTO TABLE @DATA(lt_torite_env).
    IF sy-subrc = 0.
      SORT lt_torite_env BY parent_key.
    ENDIF.

    SELECT  db_key,
            parent_key,
            item_cat,
            base_btd_tco
      FROM /scmtms/d_torite
     FOR ALL ENTRIES IN @lt_tor_root
     WHERE parent_key =  @lt_tor_root-key
       AND item_cat   = @lc_item_cat_prd
       AND ( base_btd_tco   = @lc_base_btd_tco_58 OR
             base_btd_tco   = @lc_base_btd_tco_73 )
     INTO TABLE @DATA(lt_torite_cat).
    IF sy-subrc = 0.
      SORT lt_torite_cat BY parent_key base_btd_tco.
    ENDIF.

    SELECT db_key, placavalo, Placarreta1, Placarreta2, SeqFrota4
      FROM zi_tm_sispetro
      FOR ALL ENTRIES IN @lt_tor_root
     WHERE db_key =  @lt_tor_root-key
     INTO TABLE @DATA(lt_sispetro).
    IF sy-subrc = 0.
      SORT lt_sispetro BY db_key.
      SELECT authdate,                                  "#EC CI_SEL_DEL
             parent_key
        FROM zi_tm_sispetro_nota_fiscal
        FOR ALL ENTRIES IN @lt_sispetro
        WHERE parent_key = @lt_sispetro-db_key
        INTO TABLE @DATA(lt_nfdate).
      IF sy-subrc IS INITIAL.
        SORT lt_nfdate BY parent_key.
        DELETE ADJACENT DUPLICATES FROM lt_nfdate COMPARING parent_key.
      ENDIF.
    ENDIF.

    LOOP AT lt_tor_root ASSIGNING FIELD-SYMBOL(<fs_tor_root>).

      READ TABLE lt_nfdate ASSIGNING FIELD-SYMBOL(<fs_nfdate>) WITH KEY parent_key = <fs_tor_root>-key BINARY SEARCH.
      APPEND INITIAL LINE TO rt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
*      APPEND INITIAL LINE TO <fs_data>-ordem_carregamento ASSIGNING FIELD-SYMBOL(<fs_ordem_carregamento>).
      DATA lv_data TYPE c LENGTH 20.
      IF <fs_nfdate> IS ASSIGNED.
        lv_data = <fs_nfdate>-authdate.
        CONDENSE lv_data NO-GAPS.
        <fs_data>-ordem_carregamento-data               = |{ lv_data(4) }-{ lv_data+4(2) }-{ lv_data+6(2) }|.
        CONDENSE <fs_data>-ordem_carregamento-data NO-GAPS.
      ENDIF.

      <fs_data>-ordem_carregamento-webnumerooccliente = |{ <fs_tor_root>-tor_id ALPHA = OUT }|.
      CONDENSE <fs_data>-ordem_carregamento-webnumerooccliente NO-GAPS.

      <fs_data>-ordem_carregamento-observacao = <fs_data>-ordem_carregamento-webnumerooccliente.

      READ TABLE lt_torite_env
        WITH KEY parent_key = <fs_tor_root>-key
        TRANSPORTING NO FIELDS
        BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-ordem_carregamento-coletaamostra = '1'.
      ELSE.
        <fs_data>-ordem_carregamento-coletaamostra = '2'.
      ENDIF.

      READ TABLE lt_torite_cat
        ASSIGNING FIELD-SYMBOL(<fs_torite_cat>)
        WITH KEY parent_key = <fs_tor_root>-key
        BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_torite_cat>-base_btd_tco = lc_base_btd_tco_58.
          <fs_data>-ordem_carregamento-tipo               = 'D'.
        ELSEIF <fs_torite_cat>-base_btd_tco = lc_base_btd_tco_73.
          <fs_data>-ordem_carregamento-tipo               = 'C'.
        ENDIF.
      ENDIF.

*      IF line_exists( lt_taxnum[ partner = <fs_tor_root>-tspid ] ). "#EC CI_STDSEQ
*        <fs_data>-ordem_carregamento-codtransp = lt_taxnum[ partner = <fs_tor_root>-tspid ]-taxnum. "#EC CI_STDSEQ
*      ENDIF.
      IF line_exists( lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = lc_item_cat_dri ] ).

        IF line_exists( lt_taxnum[ partner = lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = lc_item_cat_dri ]-res_id(10) ] ). "#EC CI_STDSEQ
          <fs_data>-ordem_carregamento-codtransp =  lt_taxnum[ partner = lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = lc_item_cat_dri ]-res_id(10) ]-taxnum. "#EC CI_STDSEQ
        ENDIF.
      ENDIF.



      READ TABLE lt_sispetro ASSIGNING FIELD-SYMBOL(<fs_sispetro>) WITH KEY db_key = <fs_tor_root>-key BINARY SEARCH.
      IF sy-subrc = 0. " Placarreta1, Placarreta2, SeqFrota4
        <fs_data>-ordem_carregamento-seqfrota = <fs_sispetro>-Placavalo.
        <fs_data>-ordem_carregamento-SeqFrotaCarreta = <fs_sispetro>-Placarreta1.
        <fs_data>-ordem_carregamento-SeqFrotaTrem = <fs_sispetro>-Placarreta2.
        <fs_data>-ordem_carregamento-SeqFrota4 = <fs_sispetro>-SeqFrota4.
      ENDIF.

      IF line_exists( lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ).
*      lt_tor_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ] ). "#EC CI_SORTSEQ
*        IF lt_tor_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]-erp_plant_id IS NOT INITIAL. "#EC CI_SORTSEQ
        IF lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]-erp_plant_id IS NOT INITIAL. "#EC CI_SORTSEQ
*          DATA(lv_plant_id) = lt_tor_item[ item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]-erp_plant_id. "#EC CI_SORTSEQ
          DATA(lv_plant_id) = lt_tor_item[ KEY root_itmcat root_key = <fs_tor_root>-key item_cat = /scmtms/if_tor_const=>sc_itemcat_prd ]-erp_plant_id. "#EC CI_SORTSEQ
          READ TABLE lt_empresas ASSIGNING FIELD-SYMBOL(<fs_empresas>) WITH KEY werks = lv_plant_id BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE lt_taxnum_empresas ASSIGNING FIELD-SYMBOL(<fs_taxnum_empresas>)
            WITH KEY partner = <fs_empresas>-lifnr BINARY SEARCH.
            IF sy-subrc = 0.
              <fs_data>-ordem_carregamento-cnpjempresa =
              |{ <fs_taxnum_empresas>-taxnum(2) }.{ <fs_taxnum_empresas>-taxnum+2(3) }.{ <fs_taxnum_empresas>-taxnum+5(3) }/{ <fs_taxnum_empresas>-taxnum+8(4) }-{ <fs_taxnum_empresas>-taxnum+12(2) }|.
            ENDIF.
          ENDIF.

*          IF line_exists( lt_businessplace[ branch = lv_plant_id ] ). "#EC CI_STDSEQ
*            <fs_data>-ordem_carregamento-cnpjempresa = lt_businessplace[ branch = lv_plant_id ]-businessplacestatetaxnumber. "#EC CI_STDSEQ
*          ENDIF.



        ENDIF.
      ENDIF.

      LOOP AT lt_tor_item ASSIGNING FIELD-SYMBOL(<fs_item>) USING KEY parent_key
                                                            WHERE parent_key = <fs_tor_root>-key
                                                              AND item_cat = /scmtms/if_tor_const=>sc_itemcat_prd. "#EC CI_NESTED

        APPEND INITIAL LINE TO <fs_data>-ordem_carregamento-compartimentacao_oc ASSIGNING FIELD-SYMBOL(<fs_compartimentacao>).
        <fs_compartimentacao>-codprod = |{ <fs_item>-product_id ALPHA = OUT }|.
        CONDENSE <fs_compartimentacao>-codprod NO-GAPS.




        DATA: lv_out TYPE i.
        lv_out = trunc( lt_tor_item[ key = <fs_item>-key ]-gro_vol_val ).
        <fs_compartimentacao>-qtde = lv_out.
        CONDENSE <fs_compartimentacao>-qtde NO-GAPS.
        IF line_exists( lt_tor_item[ key = <fs_item>-item_parent_key ] ).

*          <fs_compartimentacao>-qtde          = lt_tor_item[ key = <fs_item>-item_parent_key ]-gro_wei_val.


*          <fs_compartimentacao>-qtde          = lt_tor_item[ key = <fs_item>-item_parent_key ]-gro_wei_val.


          <fs_compartimentacao>-compartimento = lt_tor_item[ key = <fs_item>-item_parent_key ]-ct_seq.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD update_tor_root.

    DATA: lt_tor_root      TYPE /scmtms/t_tor_root_k,
          lt_change_fields TYPE /bobf/t_frw_name,
          lt_mod           TYPE /bobf/t_frw_modification.

    " Buscar TOR Key
    DATA(lv_tor_key) = /scmtms/cl_tor_helper_root=>return_key_for_torid( iv_torid = iv_tor_id ).

    TRY.
        /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key )->retrieve(
          EXPORTING
            iv_node_key             = /scmtms/if_tor_c=>sc_node-root                                                                      " Node
            it_key                  = VALUE #( ( key = lv_tor_key ) )                                                                     " Key Table
          IMPORTING
            et_data                 = lt_tor_root ).

        CHECK lt_tor_root IS NOT INITIAL.

        LOOP AT lt_tor_root ASSIGNING FIELD-SYMBOL(<fs_tor_root>).
          <fs_tor_root>-zz_sispetro_num = iv_num_oc.
        ENDLOOP.

        lt_change_fields = VALUE #( ( zcltm_if_tor_c=>sc_node_attribute-root-zz_sispetro_num ) ).

        /scmtms/cl_mod_helper=>mod_update_multi( EXPORTING iv_node           = /scmtms/if_tor_c=>sc_node-root                             " Node
                                                           it_data           = lt_tor_root
                                                           it_changed_fields = lt_change_fields                                           " List of Names (e.g. Fieldnames)
                                                           iv_bo_key         = /scmtms/if_tor_c=>sc_bo_key                                " Business Object
                                                 CHANGING  ct_mod            = lt_mod ).                                                  " Changes

        CHECK lt_mod IS NOT INITIAL.

        /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key )->modify( EXPORTING it_modification = lt_mod ).  " Changes
        /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->save( IMPORTING eo_change  = DATA(lo_change)                          " Interface for transaction change objects
                                                                                    eo_message = DATA(lo_message) ).                      " Interface of Message Object

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
