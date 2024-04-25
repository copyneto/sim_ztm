CLASS zcltm_seal_number DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_qty_seals
      IMPORTING
        !it_key              TYPE /bobf/t_frw_key
        !io_read             TYPE REF TO /bobf/if_frw_read
      RETURNING
        VALUE(rt_qtde_lacre) TYPE zctgtm_qtde_lacre .

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
private section.

  methods CREATE_SEAL
    importing
      !IS_TOR_ITEM type /SCMTMS/S_TOR_ITEM_TR_K
      !IO_READ type ref to /BOBF/IF_FRW_READ
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY
      !IV_SEAL_NUMBER type /SCMTMS/SEAL_NUMBER
      !IV_COMPART_NUMBER type ZE_TM_SEAL_NUMBER
    exporting
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_FAILED_KEY type /BOBF/T_FRW_KEY
    changing
      !CT_MOD type /BOBF/T_FRW_MODIFICATION optional .
  methods UPDATE_ITEM_TR
    importing
      !IT_QDTE_LACRE type ZCTGTM_QTDE_LACRE
      !IO_READ type ref to /BOBF/IF_FRW_READ optional
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY
    changing
      !CT_TOR_ITEM type /SCMTMS/T_TOR_ITEM_TR_K .
  methods MODIFY_SEAL
    importing
      !IT_CHANGED_FIELDS type /BOBF/T_FRW_NAME
      !IS_SEAL type /SCMTMS/S_TOR_SEAL_K
      !IO_READ type ref to /BOBF/IF_FRW_READ
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY
    exporting
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_FAILED_KEY type /BOBF/T_FRW_KEY
    changing
      !CT_MOD type /BOBF/T_FRW_MODIFICATION optional .
  methods PREPARE_DATA_SEAL
    importing
      !IO_READ type ref to /BOBF/IF_FRW_READ
      !IS_TOR_ITEM type /SCMTMS/S_TOR_ITEM_TR_K
      !IS_TOR_ITEM_TRK type /SCMTMS/S_TOR_ITEM_TR_K
      !IV_SEAL_NUMBER type /SCMTMS/SEAL_NUMBER
      !IV_COMPART_NUMBER type ZE_TM_SEAL_NUMBER
    changing
      !CT_DATA_SEAL type /SCMTMS/T_TOR_SEAL_K .
  methods DELETE_SEAL
    importing
      !IS_SEAL type /SCMTMS/S_TOR_SEAL_K
      !IO_READ type ref to /BOBF/IF_FRW_READ
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY
    exporting
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_FAILED_KEY type /BOBF/T_FRW_KEY
    changing
      !CT_MOD type /BOBF/T_FRW_MODIFICATION optional .
  methods UPDATE_ENVELOPE_ITEM_TR
    importing
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY
    changing
      !CT_TOR_ITEM type /SCMTMS/T_TOR_ITEM_TR_K .
ENDCLASS.



CLASS ZCLTM_SEAL_NUMBER IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    " Tabelas internas
    DATA: lt_tor_item     TYPE /scmtms/t_tor_item_tr_k,
          lt_tor_item_prd TYPE /scmtms/t_tor_item_tr_k,
          lt_tor_seal     TYPE /scmtms/t_tor_seal_k,
          lt_mod          TYPE /bobf/t_frw_modification.

    " Variáveis
    DATA: lv_field       TYPE string,
          lv_table_field TYPE string,
          lv_cont        TYPE i.

    " Constantes
    CONSTANTS: lc_table TYPE char14 VALUE '<FS_TOR_ITEM>-',
               lc_field TYPE char15 VALUE 'ZZ_SEAL_NUMBER_',
               lc_item_cat TYPE /scmtms/d_torite-item_cat VALUE 'PRD',
               lc_erp_comp_code TYPE /scmtms/d_torite-erp_comp_code VALUE '2500'.

    CLEAR: lt_tor_item,
           lt_tor_seal,
           lt_mod.

    io_read->retrieve( EXPORTING iv_node = /scmtms/if_tor_c=>sc_node-item_tr
                                 it_key  = it_key
                       IMPORTING et_data = lt_tor_item ).

    CHECK lt_tor_item IS NOT INITIAL.

    io_read->retrieve_by_association(
      EXPORTING
        iv_node        = /scmtms/if_tor_c=>sc_node-root                                                                             " Node Name
        it_key         = CORRESPONDING #( lt_tor_item MAPPING key = root_key )                                                      " Key Table
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr                                                              " Name of Association
        iv_fill_data   = abap_true                                                                                                  " Data Element for Domain BOOLE: TRUE (="X") and FALSE (=" ")
      IMPORTING
        et_data        = lt_tor_item_prd ).

*    DELETE lt_tor_item_prd WHERE zz_fu_tor_id   IS INITIAL
*                              OR zz_fu_max_util NE 100. "#EC CI_SORTSEQ
    DELETE lt_tor_item_prd WHERE zz_fu_tor_id IS INITIAL
                              OR zz_fu_max_util EQ 0. "#EC CI_SORTSEQ
    CHECK lt_tor_item_prd IS NOT INITIAL.

    LOOP AT lt_tor_item_prd ASSIGNING FIELD-SYMBOL(<fs_tor_item_prd>).
      DATA(lv_index) = sy-tabix.
      SELECT COUNT(*) FROM /scmtms/d_torite "#EC CI_SEL_NESTED
        WHERE parent_key    = @<fs_tor_item_prd>-zz_fu_db_key
          AND item_cat      = @lc_item_cat
          AND erp_comp_code = @lc_erp_comp_code.
      IF sy-subrc <> 0 AND <fs_tor_item_prd>-zz_fu_max_util NE 100.
        DELETE lt_tor_item_prd INDEX lv_index.
      ENDIF.
    ENDLOOP.

    CHECK lt_tor_item_prd IS NOT INITIAL.
    lt_tor_item[] = lt_tor_item_prd[].

    " Busca quantidade de lacres por compartimento
    DATA(lt_qtde_lacres) = get_qty_seals( it_key  = CORRESPONDING #( lt_tor_item MAPPING key = key )                                " Key Table
                                          io_read = io_read ).                                                                      " Interface to Read Data

    " Atualiza lacres no ITEM_TR
    update_item_tr( EXPORTING it_qdte_lacre = lt_qtde_lacres                                                                        " Quantidade de Lacres por Compartimento
                              io_read       = io_read
                              io_modify     = io_modify                                                                             " Interface to Change Data
                    CHANGING  ct_tor_item   = lt_tor_item ).                                                                        " Transportation Order Item

    " Atualiza envelopes de amostra
    update_envelope_item_tr( EXPORTING io_modify   = io_modify                                                                      " Interface to Change Data
                             CHANGING  ct_tor_item = lt_tor_item ).                                                                 " Transportation Order Item

    " Busca lacres caso existam
    io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-root                                     " Node Name
                                                it_key         = CORRESPONDING #( lt_tor_item MAPPING key = parent_key )            " Key Table
                                                iv_association = /scmtms/if_tor_c=>sc_association-root-seal                         " Name of Association
                                                iv_fill_data   = abap_true
                                      IMPORTING et_data        = lt_tor_seal ). "#EC CI_SORTSEQ

    LOOP AT lt_tor_item ASSIGNING FIELD-SYMBOL(<fs_tor_item>).

      IF line_exists( lt_qtde_lacres[ sequence = <fs_tor_item>-ct_seq ctype = <fs_tor_item>-ct_type ] ). "#EC CI_STDSEQ
        DATA(lv_qdte_lacre) = lt_qtde_lacres[ sequence = <fs_tor_item>-ct_seq ctype = <fs_tor_item>-ct_type ]-zz_seal_number_total. "#EC CI_STDSEQ
      ENDIF.

      CHECK lv_qdte_lacre IS NOT INITIAL.

      lv_cont = 1.
      DO lv_qdte_lacre TIMES.                            "#EC CI_NESTED

        lv_field = lc_field && lv_cont.
        lv_table_field = lc_table && lv_field.

        ASSIGN (lv_table_field) TO FIELD-SYMBOL(<fs_table_field>).

        " Criação de Lacre
        IF    <fs_table_field> IS NOT INITIAL
          AND NOT line_exists( lt_tor_seal[ zz_ref_item_ct = <fs_tor_item>-key zz_seal_number = lv_cont ] ). "#EC CI_SORTSEQ

          create_seal( EXPORTING is_tor_item       = <fs_tor_item>                                                                  " Transportation Order Item
                                 io_read           = io_read                                                                        " Interface to Read Data
                                 io_modify         = io_modify                                                                      " Interface to Change Data
                                 iv_seal_number    = <fs_table_field>                                                               " Nº do lacre
                                 iv_compart_number = CONV ze_tm_seal_number( lv_cont )                                              " Lacre por compartimento
                       IMPORTING eo_message        = eo_message                                                                     " Interface of Message Object
                                 et_failed_key     = et_failed_key
                       CHANGING  ct_mod            = lt_mod ).                                                                      " Key Table

          " Modificação de Lacre
        ELSEIF <fs_table_field> IS NOT INITIAL
          AND line_exists( lt_tor_seal[ zz_ref_item_ct = <fs_tor_item>-key zz_seal_number = lv_cont ] ). "#EC CI_SORTSEQ

          DATA(ls_seal) = lt_tor_seal[ zz_ref_item_ct = <fs_tor_item>-key zz_seal_number = lv_cont ]. "#EC CI_SORTSEQ

          IF ls_seal-seal_number NE <fs_table_field>.

            ls_seal-seal_number = <fs_table_field>.
            modify_seal( EXPORTING it_changed_fields   = VALUE #( ( /scmtms/if_tor_c=>sc_node_attribute-seal-seal_number ) )        " Transportation Order Item
                                   is_seal             = ls_seal                                                                    " Seal
                                   io_read             = io_read                                                                    " Interface to Read Data
                                   io_modify           = io_modify                                                                  " Interface to Change Data
                         IMPORTING eo_message          = eo_message                                                                 " Interface of Message Object
                                   et_failed_key       = et_failed_key                                                              " Key Table
                         CHANGING  ct_mod              = lt_mod ).
          ENDIF.

          " Deleção de Lacre
        ELSEIF <fs_table_field> IS INITIAL
          AND line_exists( lt_tor_seal[ zz_ref_item_ct = <fs_tor_item>-key zz_seal_number = lv_cont ] ). "#EC CI_SORTSEQ

          ls_seal = lt_tor_seal[ zz_ref_item_ct = <fs_tor_item>-key zz_seal_number = lv_cont ]. "#EC CI_SORTSEQ

          delete_seal( EXPORTING is_seal       = ls_seal                                                                            " Seal
                                 io_read       = io_read                                                                            " Interface to Read Data
                                 io_modify     = io_modify                                                                          " Interface to Change Data
                       IMPORTING eo_message    = eo_message                                                                         " Interface of Message Object
                                 et_failed_key = et_failed_key                                                                      " Key Table
                       CHANGING  ct_mod        = lt_mod ).

        ENDIF.
        ADD 1 TO lv_cont.
      ENDDO.

    ENDLOOP.

    CHECK lt_mod IS NOT INITIAL.

    TRY.
        io_modify->do_modify( it_modification = lt_mod ).

      CATCH /bobf/cx_frw_contrct_violation.                                                                                         " Caller violates a BOPF contract
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD create_seal.

    DATA: lt_tor_seal     TYPE /scmtms/t_tor_seal_k,
          lt_tor_item_trk TYPE /scmtms/t_tor_item_tr_k,
          lt_mod          TYPE /bobf/t_frw_modification.

    " Busca veículo
    io_read->retrieve_by_association(
      EXPORTING
        iv_node        = /scmtms/if_tor_c=>sc_node-item_tr                                                            " Node Name
        it_key         = VALUE #( ( key = is_tor_item-key ) )                                                         " Key Table
        iv_association = /scmtms/if_tor_c=>sc_association-item_tr-parent_item                                         " Name of Association
        iv_fill_data   = abap_true                                                                                    " Data Element for Domain BOOLE: TRUE (="X") and FALSE (=" ")
      IMPORTING
        et_data        = lt_tor_item_trk ).

    CHECK lt_tor_item_trk IS NOT INITIAL.

    prepare_data_seal( EXPORTING io_read           = io_read                                                          " Interface to Read Data
                                 is_tor_item       = is_tor_item                                                      " Transportation Order Item
                                 is_tor_item_trk   = lt_tor_item_trk[ 1 ]                                             " Transportation Order Item Truck
                                 iv_seal_number    = iv_seal_number                                                   " Nº do lacre
                                 iv_compart_number = iv_compart_number                                                " Lacre por compartimento
                       CHANGING  ct_data_seal      = lt_tor_seal ).                                                   " Seals

    CHECK lt_tor_seal IS NOT INITIAL.

    DATA(ls_seal) = lt_tor_seal[ 1 ].

    /scmtms/cl_mod_helper=>mod_create_multi( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-seal                " Node
                                                       it_data        = lt_tor_seal                                   " Seal Data
                                                       iv_association = /scmtms/if_tor_c=>sc_association-item_tr-seal " Association
                                                       iv_source_node = /scmtms/if_tor_c=>sc_node-item_tr             " Node
                                             CHANGING  ct_mod         = ct_mod ).                                     " Changes

  ENDMETHOD.


  METHOD get_qty_seals.

    DATA: lt_tor_item_trk   TYPE /scmtms/t_tor_item_tr_k,
          lt_tor_item_trk_s TYPE STANDARD TABLE OF /scmtms/s_tor_item_tr_k.

    DATA: lv_res_id TYPE /scmtms/res_name.

    io_read->retrieve_by_association( EXPORTING iv_node        = /scmtms/if_tor_c=>sc_node-item_tr                    " Node Name
                                                it_key         = it_key                                               " Key Table
                                                iv_association = /scmtms/if_tor_c=>sc_association-item_tr-parent_item " Name of Association
                                                iv_fill_data   = abap_true
                                      IMPORTING et_data        = lt_tor_item_trk ).

    CHECK lt_tor_item_trk IS NOT INITIAL.

    lt_tor_item_trk_s[] = lt_tor_item_trk[].
    SORT lt_tor_item_trk_s BY res_id.
    DELETE ADJACENT DUPLICATES FROM lt_tor_item_trk_s COMPARING res_id.
    DELETE lt_tor_item_trk_s WHERE res_id IS INITIAL .  "#EC CI_SORTSEQ

    IF lt_tor_item_trk_s IS NOT INITIAL.

*      SELECT cv_equipresourceroot~res_id,
*             cmprf~sequence,
*             cmprf~ctype,
*             cmprf~zz_seal_number_total
*        FROM /scmtms/cv_equipresourceroot AS cv_equipresourceroot
*        INNER JOIN /sapapo/cmprf AS cmprf ON cmprf~cmprofile EQ cv_equipresourceroot~cmprofile
*        FOR ALL ENTRIES IN @lt_tor_item_trk_s
*        WHERE cv_equipresourceroot~res_id EQ @lt_tor_item_trk_s-res_id
*        INTO TABLE @rt_qtde_lacre.

      SELECT res_id,
             sequence,
             ctype,
             zz_seal_number_total
        FROM zi_tm_get_qty_seals
        FOR ALL ENTRIES IN @lt_tor_item_trk_s
        WHERE res_id EQ @lt_tor_item_trk_s-res_id
        INTO TABLE @rt_qtde_lacre.

      IF sy-subrc NE 0.
        FREE rt_qtde_lacre.
      ENDIF.
    ENDIF.

* ---------------------------------------------------------------------------
* Limita o total de Lacres de acordo com a quantidade de campos no Cockpit de Transporte
* ---------------------------------------------------------------------------
    LOOP AT rt_qtde_lacre REFERENCE INTO DATA(ls_qtde_lacre).
      ls_qtde_lacre->zz_seal_number_total = COND #( WHEN ls_qtde_lacre->zz_seal_number_total <= zcltm_cockpit_frete_event=>gc_equi_fields-max_seal_number
                                                    THEN ls_qtde_lacre->zz_seal_number_total
                                                    ELSE zcltm_cockpit_frete_event=>gc_equi_fields-max_seal_number ).
    ENDLOOP.

    SORT rt_qtde_lacre BY res_id sequence ctype.

  ENDMETHOD.


  METHOD modify_seal.

    /scmtms/cl_mod_helper=>mod_update_single( EXPORTING is_data           = is_seal
                                                        iv_node           = /scmtms/if_tor_c=>sc_node-seal    " Node
                                                        it_changed_fields = it_changed_fields                 " List of Names (e.g. Fieldnames)
                                              CHANGING  ct_mod            = ct_mod ).                         " Changes

  ENDMETHOD.


  METHOD update_item_tr.

    CONSTANTS: lc_var_lacre TYPE char29 VALUE '<FS_TOR_ITEM>-ZZ_SEAL_NUMBER_'.

    DATA: lt_changed_fields TYPE /bobf/t_frw_name,
          lt_tor_item       TYPE /scmtms/t_tor_item_tr_k.

    DATA: lv_var_lacre              TYPE string,
*          lv_valor_lacre TYPE /scmtms/seal_number,
          lv_valor_lacre            TYPE char10,
          lv_valor_lacre_preenchido TYPE char10,
          lv_cont                   TYPE i.

    DATA: lt_seal_number TYPE SORTED TABLE OF /scmtms/seal_number   WITH UNIQUE KEY table_line.

    " Recupera todos os lacres
    LOOP AT ct_tor_item ASSIGNING FIELD-SYMBOL(<fs_tor_item>).
      IF <fs_tor_item>-zz_seal_number_1 IS NOT INITIAL.
        INSERT <fs_tor_item>-zz_seal_number_1 INTO TABLE lt_seal_number.
      ENDIF.
      IF <fs_tor_item>-zz_seal_number_2 IS NOT INITIAL.
        INSERT <fs_tor_item>-zz_seal_number_2 INTO TABLE lt_seal_number.
      ENDIF.
      IF <fs_tor_item>-zz_seal_number_3 IS NOT INITIAL.
        INSERT <fs_tor_item>-zz_seal_number_3 INTO TABLE lt_seal_number.
      ENDIF.
      IF <fs_tor_item>-zz_seal_number_4 IS NOT INITIAL.
        INSERT <fs_tor_item>-zz_seal_number_4 INTO TABLE lt_seal_number.
      ENDIF.
      IF <fs_tor_item>-zz_seal_number_5 IS NOT INITIAL.
        INSERT <fs_tor_item>-zz_seal_number_5 INTO TABLE lt_seal_number.
      ENDIF.

        IF <fs_tor_item>-zz_seal_number_6 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_6 INTO TABLE lt_seal_number.
        ENDIF.
        IF <fs_tor_item>-zz_seal_number_7 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_7 INTO TABLE lt_seal_number.
        ENDIF.
        IF <fs_tor_item>-zz_seal_number_8 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_8 INTO TABLE lt_seal_number.
        ENDIF.
      IF 1 = 2. "Remover
        IF <fs_tor_item>-zz_seal_number_9 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_9 INTO TABLE lt_seal_number.
        ENDIF.
        IF <fs_tor_item>-zz_seal_number_10 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_10 INTO TABLE lt_seal_number.
        ENDIF.
        IF <fs_tor_item>-zz_seal_number_11 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_11 INTO TABLE lt_seal_number.
        ENDIF.
        IF <fs_tor_item>-zz_seal_number_12 IS NOT INITIAL.
          INSERT <fs_tor_item>-zz_seal_number_12 INTO TABLE lt_seal_number.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Realizar regra abaixo somente quando for inserido o primeiro lacre
    CHECK lines( lt_seal_number ) = 1.

    TRY.
        lv_valor_lacre            = lt_seal_number[ 1 ].
        lv_valor_lacre            = |{ lv_valor_lacre ALPHA = IN }|.
        lv_valor_lacre_preenchido = lv_valor_lacre.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " Verifica se o código é numérico
    CHECK lv_valor_lacre CO '0123456789 '.

    LOOP AT ct_tor_item ASSIGNING <fs_tor_item>.
**********************************************************************
      IF <fs_tor_item>-res_id IS INITIAL.

        io_read->retrieve( EXPORTING iv_node = /scmtms/if_tor_c=>sc_node-item_tr
                                     it_key  = VALUE #( ( key = <fs_tor_item>-item_parent_key ) )
                           IMPORTING et_data = lt_tor_item ).

        <fs_tor_item>-res_id = VALUE #( lt_tor_item[ 1 ]-res_id OPTIONAL ).

      ENDIF.
**********************************************************************
      IF line_exists( it_qdte_lacre[ res_id = <fs_tor_item>-res_id sequence = <fs_tor_item>-ct_seq ctype = <fs_tor_item>-ct_type ] ). "#EC CI_STDSEQ
        DATA(lv_qdte_lacre) = it_qdte_lacre[ res_id = <fs_tor_item>-res_id sequence = <fs_tor_item>-ct_seq ctype = <fs_tor_item>-ct_type ]-zz_seal_number_total. "#EC CI_STDSEQ
      ENDIF.

      CHECK lv_qdte_lacre IS NOT INITIAL.

      lv_cont = 1.
      DO lv_qdte_lacre TIMES.                            "#EC CI_NESTED
        CLEAR: lv_var_lacre.

        lv_var_lacre = lc_var_lacre && lv_cont.

        ASSIGN (lv_var_lacre) TO FIELD-SYMBOL(<fs_lacre>).

        IF <fs_lacre> IS INITIAL.

          " Pega o próximo número de lacre e completa automaticamente
          lv_valor_lacre = lv_valor_lacre + 1.
          lv_valor_lacre = |{ lv_valor_lacre ALPHA = IN }|.
          <fs_lacre> = lv_valor_lacre.
          CONDENSE <fs_lacre> NO-GAPS.

        ELSE.

          <fs_lacre> = lv_valor_lacre_preenchido.

        ENDIF.

        ADD 1 TO lv_cont.
      ENDDO.
      CLEAR: lv_qdte_lacre,
             lt_changed_fields.

      IF <fs_tor_item>-zz_seal_number_1 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_changed_fields ASSIGNING FIELD-SYMBOL(<fs_changed_fields>).
        <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_1.
      ENDIF.

      IF <fs_tor_item>-zz_seal_number_2 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
        <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_2.
      ENDIF.

      IF <fs_tor_item>-zz_seal_number_3 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
        <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_3.
      ENDIF.

      IF <fs_tor_item>-zz_seal_number_4 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
        <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_4.
      ENDIF.

      IF <fs_tor_item>-zz_seal_number_5 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
        <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_5.
      ENDIF.


        IF <fs_tor_item>-zz_seal_number_6 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_6.
        ENDIF.

        IF <fs_tor_item>-zz_seal_number_7 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_7.
        ENDIF.

        IF <fs_tor_item>-zz_seal_number_8 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_8.
        ENDIF.
      IF 1 = 2. "Remover
        IF <fs_tor_item>-zz_seal_number_9 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_9.
        ENDIF.

        IF <fs_tor_item>-zz_seal_number_10 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_10.
        ENDIF.

        IF <fs_tor_item>-zz_seal_number_11 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_11.
        ENDIF.

        IF <fs_tor_item>-zz_seal_number_12 IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_changed_fields ASSIGNING <fs_changed_fields>.
          <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_seal_number_12.
        ENDIF.
      ENDIF.

      TRY.
          io_modify->update(
            iv_node           = /scmtms/if_tor_c=>sc_node-item_tr
            iv_key            = <fs_tor_item>-key
            is_data           = REF /scmtms/s_tor_item_tr_k( <fs_tor_item> )
            it_changed_fields = lt_changed_fields ).
        CATCH /bobf/cx_frw_contrct_violation.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_data_seal.

    DATA: lt_tor_item_prd TYPE /scmtms/t_tor_item_tr_k.

    DATA: ls_data_seal TYPE /scmtms/s_tor_seal_k.

    io_read->retrieve_by_association(
      EXPORTING
        iv_node        = /scmtms/if_tor_c=>sc_node-root                 " Node Name
        it_key         = VALUE #( ( key = is_tor_item-root_key ) )      " Key Table
        iv_association = /scmtms/if_tor_c=>sc_association-root-item_tr  " Name of Association
        iv_fill_data   = abap_true                                      " Data Element for Domain BOOLE: TRUE (="X") and FALSE (=" ")
      IMPORTING
        et_data        = lt_tor_item_prd ).

    DELETE lt_tor_item_prd WHERE item_parent_key <> is_tor_item-key. "#EC CI_SORTSEQ
    CHECK lt_tor_item_prd IS NOT INITIAL.

    CLEAR: ls_data_seal.
    ls_data_seal-key                 = /bobf/cl_frw_factory=>get_new_key( ).
    ls_data_seal-parent_key          = is_tor_item_trk-key.
    ls_data_seal-root_key            = is_tor_item-root_key.
    ls_data_seal-orig_ref_seal       = ls_data_seal-key.
    ls_data_seal-orig_ref_root       = is_tor_item-root_key.
    ls_data_seal-orig_ref_bo         = 'TOR'.
    ls_data_seal-seal_number         = iv_seal_number.
    ls_data_seal-zz_fu_erp_plant_id  = lt_tor_item_prd[ 1 ]-erp_plant_id.
    ls_data_seal-zz_ref_item_ct      = is_tor_item-key.
    ls_data_seal-zz_seal_number      = iv_compart_number.
    APPEND ls_data_seal TO ct_data_seal.

  ENDMETHOD.


  METHOD delete_seal.

    /scmtms/cl_mod_helper=>mod_delete_single( EXPORTING iv_node = /scmtms/if_tor_c=>sc_node-seal  " Node
                                                        iv_key  = is_seal-key                     " NodeID
                                              CHANGING  ct_mod  = ct_mod ).                       " Changes

  ENDMETHOD.


  METHOD update_envelope_item_tr.

    DATA: lt_envelope       TYPE SORTED TABLE OF ze_tm_sample_envelope WITH UNIQUE KEY table_line,
          lt_changed_fields TYPE /bobf/t_frw_name.

    DATA: lv_valor_envelope            TYPE char10,
          lv_valor_envelope_preenchido TYPE char10.

    " Recupera Envelopes de Amostra
    LOOP AT ct_tor_item ASSIGNING FIELD-SYMBOL(<fs_tor_item>).
      IF <fs_tor_item>-zz_sample_envelope IS NOT INITIAL.
        INSERT <fs_tor_item>-zz_sample_envelope INTO TABLE lt_envelope.
      ENDIF.
    ENDLOOP.

    " Realizar regra abaixo somente quando for inserido o primeiro envelope
    CHECK lines( lt_envelope ) = 1.

    TRY.
        lv_valor_envelope            = lt_envelope[ 1 ].
        lv_valor_envelope            = |{ lv_valor_envelope ALPHA = IN }|.
        lv_valor_envelope_preenchido = lv_valor_envelope.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    CONSTANTS:
      lc_numbers TYPE string VALUE '0123456789 '.
    " Verifica se o código é numérico
    CHECK lv_valor_envelope CO lc_numbers.

    CLEAR: lt_changed_fields.
    APPEND INITIAL LINE TO lt_changed_fields ASSIGNING FIELD-SYMBOL(<fs_changed_fields>).
    <fs_changed_fields> = zcltm_if_tor_c=>sc_node_attribute-item_tr-zz_sample_envelope.

    LOOP AT ct_tor_item ASSIGNING <fs_tor_item>.
      IF <fs_tor_item>-zz_sample_envelope IS INITIAL.

        " Pega o próximo número de envelope e completa automaticamente
        lv_valor_envelope = lv_valor_envelope + 1.
        lv_valor_envelope = |{ lv_valor_envelope ALPHA = IN }|.
        <fs_tor_item>-zz_sample_envelope = lv_valor_envelope.
        CONDENSE <fs_tor_item>-zz_sample_envelope NO-GAPS.

      ELSE.

        <fs_tor_item>-zz_sample_envelope = lv_valor_envelope_preenchido.

      ENDIF.

      TRY.
          " Atualiza campo envelope
          io_modify->update(
            iv_node           = /scmtms/if_tor_c=>sc_node-item_tr
            iv_key            = <fs_tor_item>-key
            is_data           = REF /scmtms/s_tor_item_tr_k( <fs_tor_item> )
            it_changed_fields = lt_changed_fields ).

        CATCH /bobf/cx_frw_contrct_violation.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
