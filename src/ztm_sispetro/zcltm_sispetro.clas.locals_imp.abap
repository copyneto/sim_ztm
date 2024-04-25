CLASS lcl_lhc_ZI_TM_SISPETRO DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_tm_sispetro RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_tm_sispetro RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_sispetro RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_tm_sispetro.

    METHODS rba_Loga FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sispetro\_Loga FULL result_requested RESULT result LINK association_links.

*    METHODS cba_Loga FOR MODIFY
*      IMPORTING entities_cba FOR CREATE zi_tm_sispetro\_Loga.

    METHODS criar FOR MODIFY
      IMPORTING keys FOR ACTION zi_tm_sispetro~criar.

    METHODS rba_notafiscal FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sispetro\_NotaFiscal FULL result_requested RESULT result LINK association_links.

    METHODS rba_compartimentos FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sispetro\_Compartimentos FULL result_requested RESULT result LINK association_links.

    METHODS rba_lacres FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sispetro\_Lacres FULL result_requested RESULT result LINK association_links.

*    METHODS cba_torite FOR MODIFY
*      IMPORTING entities_cba FOR CREATE zi_tm_sispetro\_torite.
    TYPES:
      ty_reported TYPE RESPONSE FOR REPORTED EARLY zi_tm_sispetro.

    CONSTANTS:
      BEGIN OF gc_cds,
        motoristas TYPE string VALUE 'ZI_TM_SISPETRO'   ##NO_TEXT,
        doc_add    TYPE string VALUE 'ZI_TM_DOC_ADD'    ##NO_TEXT,
      END OF gc_cds,
      BEGIN OF gc_reported,
        msg     TYPE string VALUE '%msg',
        element TYPE string VALUE '%element',
      END OF gc_reported.

    "! Constrói mensagens retorno do aplicativo
    METHODS build_reported
      IMPORTING
        !it_return   TYPE bapiret2_t
      EXPORTING
        !es_reported TYPE ty_reported.
ENDCLASS.

CLASS lcl_lhc_ZI_TM_SISPETRO IMPLEMENTATION.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD rba_Loga.
    RETURN.
  ENDMETHOD.

*  METHOD cba_Loga.
*    RETURN.
*  ENDMETHOD.

  METHOD criar.
    CONSTANTS:
      lc_item_type_prd TYPE /scmtms/tor_item_type VALUE 'PRD',
      lc_doctyp_05     TYPE j_1bnfdoc-doctyp VALUE '5',
      lc_type_error    TYPE bapi_mtype VALUE 'E',
      lc_id            TYPE symsgid    VALUE 'ZTM_COCKPIT_FRETE',
      lc_number_021    TYPE symsgno    VALUE '021',
      lc_number_020    TYPE symsgno    VALUE '020'.



    DATA:
      lt_return_validacao TYPE bapiret2_t.
    DATA(lt_key) = VALUE /bobf/t_frw_key( FOR <fs_key> IN keys
                                        ( key = <fs_key>-db_key ) ).


    SELECT _torRot~tor_id, _docFlow~BR_NotaFiscal, _doc~nfenum AS Nota, _docActive~nfnum9 AS NotaAprovada
     FROM /scmtms/d_torrot AS _torRot
    INNER JOIN @lt_key AS _keys
    ON _torRot~db_key = _keys~key
    INNER JOIN /scmtms/d_torite AS _torite
    ON _torRot~db_key = _torite~parent_key
    LEFT OUTER JOIN I_BR_NFDocumentFlow_C AS _docFlow
    ON _docFlow~PredecessorReferenceDocument = substring( _torite~base_btd_id,26,10 )
    LEFT OUTER JOIN  j_1bnfdoc AS _doc
    ON _doc~docnum = _docFlow~BR_NotaFiscal
    AND _doc~doctyp <> @lc_doctyp_05
    AND _doc~cancel <> @abap_true
    LEFT OUTER JOIN j_1bnfe_active AS _docActive
    ON _docActive~docnum =  _doc~docnum
    AND _docActive~cancel <> @abap_true
    WHERE _torite~item_type = @lc_item_type_prd
    GROUP BY tor_id, _docFlow~BR_NotaFiscal, _doc~nfenum, _docActive~nfnum9
    INTO TABLE @DATA(lt_dados).

    IF sy-subrc <> 0.
      APPEND VALUE #(
        type   = lc_type_error
        id     = lc_id
        number = lc_number_021
*        message =
*        message_v1
      ) TO lt_return_validacao.
    ENDIF.

    LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
      IF ( <fs_dados>-BR_NotaFiscal IS INITIAL OR <fs_dados>-BR_NotaFiscal = 0000000000 ) OR
         ( <fs_dados>-Nota IS INITIAL ).
        APPEND VALUE #(
          type   = lc_type_error
          id     = lc_id
          number = lc_number_021
*          message =
*          message_v1
        ) TO lt_return_validacao.
        EXIT.
      ENDIF.
      IF ( <fs_dados>-nota IS NOT INITIAL ) AND
         ( <fs_dados>-notaaprovada IS INITIAL ).
        APPEND VALUE #(
          type   = lc_type_error
          id     = lc_id
          number = lc_number_020
*          message =
          message_v1 = |{ <fs_dados>-nota ALPHA = OUT }|
          message_v2 = |{ <fs_dados>-tor_id ALPHA = OUT }|
        ) TO lt_return_validacao.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lt_return_validacao IS INITIAL.
      DATA(lt_return) = NEW zcltm_sispetro_send_data( )->exec( EXPORTING it_key = lt_key ).
      APPEND LINES OF lt_Return TO lt_return_validacao.
    ENDIF.

    build_reported( EXPORTING it_return   = lt_return_validacao
                    IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

*  METHOD cba_Torite.
*    RETURN.
*  ENDMETHOD.

  METHOD rba_compartimentos.
    RETURN.
  ENDMETHOD.

  METHOD rba_lacres.
    RETURN.
  ENDMETHOD.

  METHOD rba_notafiscal.
    RETURN.
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
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-zi_tm_sispetro.
*        WHEN gc_cds-doc_add.
*          CREATE DATA lo_dataref TYPE LINE OF ty_reported-zi_tm_doc_add.
        WHEN OTHERS.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-zi_tm_sispetro.
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
          es_reported-zi_tm_sispetro[] = VALUE #( BASE es_reported-zi_tm_sispetro[] ( CORRESPONDING #( <fs_cds> ) ) ).
*        WHEN gc_cds-doc_add.
*          es_reported-zi_tm_doc_add[]    = VALUE #( BASE es_reported-zi_tm_doc_add[] ( CORRESPONDING #( <fs_cds> ) ) ).
        WHEN OTHERS.
          es_reported-zi_tm_sispetro[] = VALUE #( BASE es_reported-zi_tm_sispetro[] ( CORRESPONDING #( <fs_cds> ) ) ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_lhc_ZI_TM_SISPETRO_LOG_ALL DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_sispetro_log_all RESULT result.

    METHODS rba_Pai FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sispetro_log_all\_Pai FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SISPETRO_LOG_ALL IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Pai.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SISPETRO_NF DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_sispetro_nota_fiscal RESULT result.

    METHODS rba_Pai FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sispetro_nota_fiscal\_paiNotaFiscal FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SISPETRO_NF IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Pai.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_ORD_CARREGA_COMP DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_ordem_carregamento_comp RESULT result.

    METHODS rba_PaiComp FOR READ
      IMPORTING keys_rba FOR READ zi_tm_ordem_carregamento_comp\_paiComp FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_ORD_CARREGA_COMP IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_PaiComp.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lhc_zi_tm_ord_carreg_lacre DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_ordem_carreg_lista_lacre RESULT result.

    METHODS rba_Pailacre FOR READ
      IMPORTING keys_rba FOR READ zi_tm_ordem_carreg_lista_lacre\_paiLacre FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_lhc_zi_tm_ord_carreg_lacre IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Pailacre.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lsc_ZI_TM_SISPETRO DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_lsc_ZI_TM_SISPETRO IMPLEMENTATION.

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
