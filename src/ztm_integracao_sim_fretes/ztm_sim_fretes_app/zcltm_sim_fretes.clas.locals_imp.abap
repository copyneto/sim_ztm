CLASS lcl_lhc_ZI_TM_SIM_FRETES DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_tm_sim_fretes RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_sim_fretes RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_tm_sim_fretes.

    METHODS rba_Log FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sim_fretes\_Log FULL result_requested RESULT result LINK association_links.

    METHODS rba_Nfe FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sim_fretes\_Nfe FULL result_requested RESULT result LINK association_links.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_tm_sim_fretes RESULT result.

    METHODS cadastrar FOR MODIFY
      IMPORTING keys FOR ACTION zi_tm_sim_fretes~cadastrar.

    METHODS agrupar FOR MODIFY
      IMPORTING keys FOR ACTION zi_tm_sim_fretes~agrupar.

    METHODS desagrupar FOR MODIFY
      IMPORTING keys FOR ACTION zi_tm_sim_fretes~desagrupar.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SIM_FRETES IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD rba_Log.
    RETURN.
  ENDMETHOD.

  METHOD rba_Nfe.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD cadastrar.
    RETURN.
  ENDMETHOD.

  METHOD agrupar.
    IF lines( keys ) < 2.
      reported-zi_tm_sim_fretes = VALUE #( (
        %msg = new_message(
          id       = 'ZTM_SIM_FRETES'
          number   = '001'
          severity = if_abap_behv_message=>severity-error
      ) ) ).
      RETURN.
    ENDIF.

    SELECT ctcnumero, ctcserie, fornecedorCnpj, docContabilComp
    FROM zi_tm_sim_fretes
    INTO TABLE @DATA(lt_sim_fretes)
    FOR ALL ENTRIES IN @keys
    WHERE CtcNumero = @keys-ctcnumero
      AND ctcserie = @keys-ctcserie.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DATA(lv_erro_doc_cont_comp) = abap_false.
    DATA(lv_erro_fornecedor) = abap_false.
    DATA(lv_FornecedorCnpj) = lt_sim_fretes[ 1 ]-FornecedorCnpj.
    LOOP AT lt_sim_fretes ASSIGNING FIELD-SYMBOL(<fs_sim_fretes>).
      IF <fs_sim_fretes>-docContabilComp IS NOT INITIAL.
        lv_erro_doc_cont_comp = abap_true.
        EXIT.
      ENDIF.
      IF <fs_sim_fretes>-FornecedorCnpj <> lv_FornecedorCnpj.
        lv_erro_fornecedor = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_erro_doc_cont_comp = abap_true.
      reported-zi_tm_sim_fretes = VALUE #( (
        %msg = new_message(
          id       = 'ZTM_SIM_FRETES'
          number   = '003'
          severity = if_abap_behv_message=>severity-error
      ) ) ).
      RETURN.
    ENDIF.
    IF lv_erro_fornecedor = abap_true.
      reported-zi_tm_sim_fretes = VALUE #( (
        %msg = new_message(
          id       = 'ZTM_SIM_FRETES'
          number   = '002'
          severity = if_abap_behv_message=>severity-error
      ) ) ).
      RETURN.
    ENDIF.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_key>).
      DATA(lt_return) = NEW zclfi_agrupador_faturas( )->agrupar_fatura(
        iv_ctcnumero      = <fs_key>-ctcnumero
        iv_ctcserie       = <fs_key>-ctcserie
        iv_dataBasePopUp  = <fs_key>-%param-dataBasePopUp
        iv_condPgtoPopUp  = <fs_key>-%param-condPgtoPopUp
        iv_formaPgtoPopUp = <fs_key>-%param-formaPgtoPopUp
      ).
      reported-zi_tm_sim_fretes = VALUE #( BASE reported-zi_tm_sim_fretes
        FOR ls_msg IN lt_return (
          %key = <fs_key>-%key "CORRESPONDING #( <fs_sim_fretes> )
          %msg = new_message(
            id       = ls_msg-id
            number   = ls_msg-number
            severity = CONV #( ls_msg-type )
            v1       = ls_msg-message_v1
            v2       = ls_msg-message_v2
            v3       = ls_msg-message_v3
            v4       = ls_msg-message_v4
          )
        )
      ).
    ENDLOOP.

  ENDMETHOD.

  METHOD desagrupar.
    CHECK keys IS NOT INITIAL.

    SELECT ctcnumero, ctcserie, docContabilComp
    FROM zi_tm_sim_fretes
    INTO TABLE @DATA(lt_sim_fretes)
    FOR ALL ENTRIES IN @keys
    WHERE CtcNumero = @keys-ctcnumero
      AND ctcserie = @keys-ctcserie.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    SORT lt_sim_fretes BY docContabilComp.
    DELETE ADJACENT DUPLICATES FROM lt_sim_fretes COMPARING docContabilComp. "#EC CI_SEL_DEL

    IF line_exists( lt_sim_fretes[ docContabilComp = '' ] ). "#EC CI_STDSEQ
      reported-zi_tm_sim_fretes = VALUE #( (
        %msg = new_message(
          id       = 'ZTM_SIM_FRETES'
          number   = '004'
          severity = if_abap_behv_message=>severity-error
      ) ) ).
      RETURN.
    ENDIF.

    LOOP AT lt_sim_fretes ASSIGNING FIELD-SYMBOL(<fs_lt_sim_fretes>).
      DATA(lt_return) = NEW zclfi_agrupador_faturas( )->desagrupar_fatura(
        iv_bukrs = <fs_lt_sim_fretes>-docContabilComp(4)
        iv_augbl = <fs_lt_sim_fretes>-docContabilComp+4(10)
        iv_gjahr = CONV #( <fs_lt_sim_fretes>-docContabilComp+14(4) )
      ).

      reported-zi_tm_sim_fretes = VALUE #( BASE reported-zi_tm_sim_fretes
        FOR ls_msg IN lt_return (
          %key = CORRESPONDING #( <fs_lt_sim_fretes> )
          %msg = new_message(
            id       = ls_msg-id
            number   = ls_msg-number
            severity = CONV #( ls_msg-type )
            v1       = ls_msg-message_v1
            v2       = ls_msg-message_v2
            v3       = ls_msg-message_v3
            v4       = ls_msg-message_v4
          )
        )
      ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SIM_FRETES_LOG DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_sim_fretes_log RESULT result.

    METHODS rba_Pai FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sim_fretes_log\_Pai FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SIM_FRETES_LOG IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Pai.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SIM_FRETES_NFE DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_tm_sim_fretes_nfe RESULT result.

    METHODS rba_Pai FOR READ
      IMPORTING keys_rba FOR READ zi_tm_sim_fretes_nfe\_Pai FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_lhc_ZI_TM_SIM_FRETES_NFE IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Pai.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lsc_ZI_TM_SIM_FRETES DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_lsc_ZI_TM_SIM_FRETES IMPLEMENTATION.

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
