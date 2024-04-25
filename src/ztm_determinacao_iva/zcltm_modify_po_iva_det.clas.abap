CLASS zcltm_modify_po_iva_det DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_data_det_iva,
        cenario        TYPE ze_cenario,
        uforigem       TYPE regio,
        ufdestino      TYPE regio,
        uftransp       TYPE regio,
        transportadora TYPE ze_transportadora,
        clientefornec  TYPE ze_clientefornec,
        iva            TYPE mwskz,
      END OF ty_data_det_iva .

    DATA gs_data_det_iva TYPE ty_data_det_iva .
    CONSTANTS: BEGIN OF gc_cenario,
                 modulo TYPE ze_param_modulo VALUE 'TM',
                 chave1 TYPE ze_param_chave1 VALUE 'DETERMINA_IVA',
               END OF gc_cenario.

    METHODS det_iva
      IMPORTING
        !it_tor_root    TYPE /scmtms/t_tor_root_k
        !it_item_tr     TYPE /scmtms/t_tor_item_tr_k
      CHANGING
        !ct_po_items    TYPE bapimepoitem_tp
        !ct_po_itemsx   TYPE bapimepoitemx_tp
        !ct_po_services TYPE bapiesllc_tp .
protected section.
private section.

  methods GET_IVA
    changing
      !CS_DATA_DET_IVA type TY_DATA_DET_IVA .
  methods GET_CENARIO
    importing
      !IT_ITEM_TR type /SCMTMS/T_TOR_ITEM_TR_K
    changing
      !CT_CENARIO type ZE_CENARIO .
  methods GET_CENARIO_2
    importing
      !IS_ROOT type /SCMTMS/S_TOR_ROOT_K
    changing
      !CT_CENARIO type ZE_CENARIO .
ENDCLASS.



CLASS ZCLTM_MODIFY_PO_IVA_DET IMPLEMENTATION.


  METHOD det_iva.

    DATA: lt_stop_first_and_last TYPE /scmtms/t_tor_stop_k,
          lt_bo_loc_adr_loc      TYPE /scmtms/t_bo_loc_addr_detailsk.

*    CHECK sy-uname EQ 'RPORTES' OR
*          sy-uname EQ 'TVALADA'.

    CLEAR: gs_data_det_iva.
    DATA(lo_srvmgr_tor) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key ).

    CHECK it_tor_root IS NOT INITIAL.
    DATA(ls_tor_root) = VALUE #( it_tor_root[ 1 ] OPTIONAL ).

    " Cenário
*    get_cenario( EXPORTING it_item_tr = it_item_tr                                                                                    " Transportation Order Item
*                 CHANGING  ct_cenario = gs_data_det_iva-cenario ).                                                                    " Cenário

    get_cenario_2( EXPORTING is_root   = ls_tor_root
                   CHANGING ct_cenario = gs_data_det_iva-cenario ).                                                                   " Cenário

    CHECK gs_data_det_iva-cenario IS NOT INITIAL.

    " Transportadora
    gs_data_det_iva-transportadora = ls_tor_root-tspid.

    " UF Transportadora
    SELECT SINGLE regio
      FROM lfa1
      INTO gs_data_det_iva-uftransp
     WHERE lifnr EQ ls_tor_root-tspid.

    " Cliente/Fornecedor
    IF ls_tor_root-consigneeid IS NOT INITIAL.
      gs_data_det_iva-clientefornec = ls_tor_root-consigneeid.
    ELSEIF ls_tor_root-shipperid IS NOT INITIAL.
      gs_data_det_iva-clientefornec = ls_tor_root-shipperid.
    ENDIF.
    TRY.

        lo_srvmgr_tor->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-root                             " Node
                                                          it_key         = CORRESPONDING #( it_tor_root MAPPING key = key )           " Key Table
                                                          iv_association = /scmtms/if_tor_c=>sc_association-root-stop_first_and_last  " Association
                                                          iv_fill_data   = abap_true                                                  " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
                                                IMPORTING et_data        = lt_stop_first_and_last
                                                          et_target_key  = DATA(lt_stop_key) ).

        CHECK lt_stop_first_and_last IS NOT INITIAL.

        lo_srvmgr_tor->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_tor_c=>sc_node-stop                             " Node
                                                          it_key         = lt_stop_key                                                " Key Table
                                                          iv_association = /scmtms/if_tor_c=>sc_association-stop-bo_loc_adr_loc       " Association
                                                          iv_fill_data   = abap_true                                                  " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
                                                IMPORTING et_data        = lt_bo_loc_adr_loc ).

        CHECK lt_bo_loc_adr_loc IS NOT INITIAL.

        LOOP AT lt_stop_first_and_last ASSIGNING FIELD-SYMBOL(<fs_stop_first_and_last>).
          CASE <fs_stop_first_and_last>-stop_cat.
            WHEN /scmtms/if_tor_const=>sc_tor_stop_cat-inbound.

              " UF Destino
              gs_data_det_iva-ufdestino = VALUE #( lt_bo_loc_adr_loc[ location_id = <fs_stop_first_and_last>-log_locid ]-region OPTIONAL ). "#EC CI_STDSEQ

            WHEN /scmtms/if_tor_const=>sc_tor_stop_cat-outbound.

              " UF Origem
              gs_data_det_iva-uforigem = VALUE #( lt_bo_loc_adr_loc[ location_id = <fs_stop_first_and_last>-log_locid ]-region OPTIONAL ). "#EC CI_STDSEQ

          ENDCASE.
        ENDLOOP.

        " Seleção de IVA
        get_iva( CHANGING cs_data_det_iva = gs_data_det_iva ).

        CHECK gs_data_det_iva-iva IS NOT INITIAL.

        SORT: ct_po_itemsx BY po_item.
        LOOP AT ct_po_items ASSIGNING FIELD-SYMBOL(<fs_po_item>).
          READ TABLE ct_po_itemsx ASSIGNING FIELD-SYMBOL(<fs_po_itemx>)
            WITH KEY po_item = <fs_po_item>-po_item BINARY SEARCH.
          CHECK sy-subrc EQ 0.

          <fs_po_item>-tax_code  = gs_data_det_iva-iva.
          <fs_po_itemx>-tax_code = abap_true.
        ENDLOOP.

        LOOP AT ct_po_services ASSIGNING FIELD-SYMBOL(<fs_po_services>).
          <fs_po_services>-tax_code = gs_data_det_iva-iva.
        ENDLOOP.

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        RETURN.
    ENDTRY.


  ENDMETHOD.


  METHOD get_cenario.

    " Variáveis
    DATA: lr_cenario TYPE RANGE OF char4.

    " Tabelas Internas
    DATA: lt_sel_material TYPE RANGE OF matnr.

    TRY.

        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = gc_cenario-modulo
            iv_chave1 = gc_cenario-chave1
          IMPORTING
            et_range  = lr_cenario ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

    lt_sel_material = VALUE #( FOR <fs_material> IN it_item_tr WHERE
                             ( product_id IS NOT INITIAL )
                             ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                               option = /bobf/if_conf_c=>sc_sign_equal
                               low    = CONV matnr( <fs_material>-product_id ) ) ). "#EC CI_SORTSEQ

    CHECK lt_sel_material IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_sel_material COMPARING ALL FIELDS.

    SELECT mtart
      FROM mara
      INTO TABLE @DATA(lt_mara)
     WHERE matnr IN @lt_sel_material.                   "#EC CI_SEL_DEL

    CHECK sy-subrc IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_mara COMPARING mtart.

    ct_cenario = VALUE #( lr_cenario[ low = lt_mara[ 1 ]-mtart ]-high OPTIONAL ). "#EC CI_STDSEQ


  ENDMETHOD.


  METHOD get_iva.

    CHECK cs_data_det_iva IS NOT INITIAL.

    SELECT SINGLE iva
      FROM zttm_determ_iva
      INTO cs_data_det_iva-iva
     WHERE cenario        EQ cs_data_det_iva-cenario
       AND uforigem       EQ cs_data_det_iva-uforigem
       AND ufdestino      EQ cs_data_det_iva-ufdestino
       AND uftransp       EQ cs_data_det_iva-uftransp
       AND transportadora EQ cs_data_det_iva-transportadora
       AND clientefornec  EQ cs_data_det_iva-clientefornec.

    CHECK sy-subrc IS NOT INITIAL.

    SELECT SINGLE iva
      FROM zttm_determ_iva
      INTO cs_data_det_iva-iva
     WHERE cenario        EQ cs_data_det_iva-cenario
       AND uforigem       EQ cs_data_det_iva-uforigem
       AND ufdestino      EQ cs_data_det_iva-ufdestino
       AND uftransp       EQ cs_data_det_iva-uftransp
       AND transportadora EQ cs_data_det_iva-transportadora.

    CHECK sy-subrc IS NOT INITIAL.

    SELECT SINGLE iva
      FROM zttm_determ_iva
      INTO cs_data_det_iva-iva
     WHERE cenario        EQ cs_data_det_iva-cenario
       AND uforigem       EQ cs_data_det_iva-uforigem
       AND ufdestino      EQ cs_data_det_iva-ufdestino
       AND uftransp       EQ cs_data_det_iva-uftransp.

    CHECK sy-subrc IS NOT INITIAL.

  ENDMETHOD.


  METHOD GET_CENARIO_2.

    " Variáveis
    DATA: lr_cenario TYPE RANGE OF char4.

    " Tabelas Internas
    DATA: lt_sel_material TYPE RANGE OF matnr.

    TRY.

        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = gc_cenario-modulo
            iv_chave1 = gc_cenario-chave1
          IMPORTING
            et_range  = lr_cenario ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

    ct_cenario = lr_cenario[ low = is_root-tor_type ]-high.


  ENDMETHOD.
ENDCLASS.
