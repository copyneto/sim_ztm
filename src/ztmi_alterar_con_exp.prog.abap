*&---------------------------------------------------------------------*
*& Include ZTMI_ALTERAR_CON_EXP
*&---------------------------------------------------------------------*
CONSTANTS:
  gc_sales_order_create TYPE syst_tcode VALUE 'VA01',
  gc_sales_order_change TYPE syst_tcode VALUE 'VA02',
  gc_cond_exp_zf        TYPE vsbed VALUE 'ZF',
  gc_cond_exp_01        TYPE vsbed VALUE '01',
  gc_progname           TYPE dbglprog VALUE 'CL_SD_API_SALES_ORDER_DPC_EXT=CP',
  gc_eventtype          TYPE dbglevtype VALUE 'METH',
  gc_eventname          TYPE dbglevent VALUE '/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY'.


DATA: lt_abap_callstack TYPE abap_callstack,
      lt_callstack      TYPE sys_callst.

CONSTANTS:
  BEGIN OF gc_parametros,
    modulo TYPE ztca_param_mod-modulo VALUE 'TM',
    chave1 TYPE ztca_param_par-chave1 VALUE 'TIPO_EXPEDICAO',
    chave2 TYPE ztca_param_par-chave2 VALUE 'EMPRESAS',
    chave3 TYPE ztca_param_par-chave3 VALUE '',
  END OF gc_parametros .
DATA:
  lr_empresas TYPE RANGE OF  t001k-bukrs.
TRY.


    DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).

    CLEAR lr_empresas.

    lr_param->m_get_range( EXPORTING iv_modulo = gc_parametros-modulo
                                     iv_chave1 = gc_parametros-chave1
                                     iv_chave2 = gc_parametros-chave2
                           IMPORTING et_range  = lr_empresas ).

    IF sy-tcode IS INITIAL.
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          callstack    = lt_abap_callstack
          et_callstack = lt_callstack.

      SORT lt_callstack BY progname eventtype eventname.

      READ TABLE lt_callstack TRANSPORTING NO FIELDS
        WITH KEY progname  = gc_progname
                 eventtype = gc_eventtype
                 eventname = gc_eventname BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_api) = abap_true.
      ENDIF.
    ENDIF.

    DATA: lv_modfrete TYPE j_1bmodfrete_det-modfrete.

    SELECT SINGLE modfrete
      FROM j_1bmodfrete_det
      INTO lv_modfrete
      WHERE inco1 = vbkd-inco1.
   IF sy-subrc = 0.
     DATA(lv_encontrou) = abap_true.
   ENDIF.

    vbak-vsbed = COND #(
      WHEN ( sy-tcode = gc_sales_order_create OR sy-tcode = gc_sales_order_change OR lv_api EQ abap_true ) AND vbak-bukrs_vf IN lr_empresas
      THEN SWITCH #( lv_modfrete
                    WHEN '0' THEN gc_cond_exp_01
                    WHEN '1' THEN gc_cond_exp_zf
                    WHEN '9' THEN ''
                    ELSE vbak-vsbed )
      ELSE vbak-vsbed
    ).

***********************************************************************
*    IF ( sy-tcode = gc_sales_order_create OR sy-tcode = gc_sales_order_change OR lv_api EQ abap_true ) AND vbkd-inco1 IS NOT INITIAL.
*      CONSTANTS:
*        BEGIN OF gc_parametros_cond_exp,
*          modulo TYPE ztca_param_mod-modulo VALUE 'TM',
*          chave1 TYPE ztca_param_par-chave1 VALUE 'DETERMINA_CONDICAO_EXPEDICAO',
*          chave2 TYPE ztca_param_par-chave2 VALUE 'REMESSA_E_ORDEM_VENDAS',
*          chave3 TYPE ztca_param_par-chave3 VALUE '',
*        END OF gc_parametros_cond_exp .
*      DATA:
*        lr_vsbed TYPE RANGE OF char3.
*      TRY.
*          DATA(lr_param_cond_exp) = zclca_tabela_parametros=>get_instance( ).
*          CLEAR lr_vsbed.
*          lr_param_cond_exp->m_get_range( EXPORTING iv_modulo = gc_parametros_cond_exp-modulo
*                                           iv_chave1 = gc_parametros_cond_exp-chave1
*                                           iv_chave2 = gc_parametros_cond_exp-chave2
*                                 IMPORTING et_range  = lr_vsbed ).
*        CATCH zcxca_tabela_parametros.
*      ENDTRY.
*      READ TABLE lr_vsbed ASSIGNING FIELD-SYMBOL(<fs_vsbed>) WITH KEY low = vbkd-inco1. "#EC CI_STDSEQ
*      IF sy-subrc = 0.
*        vbak-vsbed = <fs_vsbed>-high.
*      ENDIF.
*    ENDIF.
***********************************************************************
  CATCH zcxca_tabela_parametros.
    lv_encontrou = abap_false.
ENDTRY.

*CONSTANTS:
*  gc_bukrs_2000         TYPE vbak-bukrs_vf VALUE '2000',
*  gc_bukrs_2500         TYPE vbak-bukrs_vf VALUE '2500',
*  gc_bukrs_3000         TYPE vbak-bukrs_vf VALUE '3000',
*  gc_incoterms_fob      TYPE inco1 VALUE 'FOB',
*vbak-vsbed = COND #(
*  WHEN ( sy-tcode = gc_sales_order_create OR sy-tcode = gc_sales_order_change OR lv_api EQ abap_true ) AND
*         vbkd-inco1 = gc_incoterms_fob AND
*       ( vbak-bukrs_vf = gc_bukrs_2000 OR vbak-bukrs_vf = gc_bukrs_2500 OR vbak-bukrs_vf = gc_bukrs_3000 )
*  THEN gc_cond_exp_zf
*  ELSE vbak-vsbed
*).
