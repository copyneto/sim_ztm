CLASS zcltm_print_amostra DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_printer TYPE rspopname OPTIONAL.

    METHODS call_form
      IMPORTING
        !it_ordem     TYPE /scmtms/t_tor_id
        !iv_return    TYPE abap_bool OPTIONAL
        iv_docnum     TYPE j_1bdocnum OPTIONAL
      RETURNING
        VALUE(rv_pdf) TYPE zctgtm_return_ce_print_amst
      EXCEPTIONS
        no_envelope .
    METHODS call_form_danfe
      IMPORTING
        !iv_docnum TYPE j_1bdocnum
        !iv_vstel  TYPE vstel .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_ordfrete,
        tor_id TYPE /scmtms/tor_id,
      END OF ty_ordfrete .
    TYPES:
      BEGIN OF ty_agrp_lacre,
        root_key TYPE /bobf/conf_key,
        lacres   TYPE string,
      END OF ty_agrp_lacre .

    "CONSTANTS gc_print_name TYPE fpname VALUE 'ZAFTM_ORDEM_CARREGAMENTO' ##NO_TEXT.
    CONSTANTS gc_print_name TYPE fpname VALUE 'ZAFTM_COLETA_AMOSTRA' ##NO_TEXT.

    METHODS get_data
      IMPORTING
        !it_ordem           TYPE /scmtms/t_tor_id
        iv_docnum           TYPE j_1bdocnum OPTIONAL
      RETURNING
        VALUE(rt_print_tab) TYPE zctgtm_print_amostra .
    METHODS get_function_name
      RETURNING
        VALUE(rv_function) TYPE funcname .

    DATA gv_printer TYPE rspopname.
ENDCLASS.



CLASS zcltm_print_amostra IMPLEMENTATION.


  METHOD call_form.
    CONSTANTS:
      lc_tddest   TYPE rspopname  VALUE 'LP01',
      lc_printer  TYPE fpmedium   VALUE 'PRINTER',
      lc_getpdf   TYPE fpgetpdf   VALUE 'M',
      lc_bumode   TYPE fpbumode   VALUE 'M',
      lc_assemble TYPE fpassemble VALUE 'S'.

    DATA:
      ls_outputparams       TYPE sfpoutputparams,
      ls_params             TYPE sfpdocparams,
      ls_joboutput          TYPE sfpjoboutput,
      ls_control_parameters TYPE ssfctrlop,
      ls_print_amstrg       TYPE zstm_print_amostra.

    DATA lt_tfpcontent TYPE tfpcontent.
    DATA lo_pdf_merger TYPE REF TO cl_rspo_pdf_merge.

    DATA(lv_function)     = me->get_function_name( ).
    DATA(lt_print_amstrg) = me->get_data( EXPORTING it_ordem = it_ordem[] iv_docnum = iv_docnum ).

    IF me->gv_printer IS NOT INITIAL.
      ls_outputparams-dest     = me->gv_printer.
      ls_outputparams-reqimm   = abap_true.
    ELSE.
      ls_outputparams-dest     = lc_tddest.
      ls_outputparams-getpdf   = lc_getpdf.
      ls_outputparams-bumode   = lc_getpdf.
      ls_outputparams-assemble = lc_assemble.

      ls_control_parameters-no_open  = abap_true.
      ls_control_parameters-no_close = abap_true.
    ENDIF.

    ls_outputparams-nodialog = abap_true.
    ls_outputparams-device   = lc_printer.

*    IF iv_return = abap_true.
*      ls_outputparams-device   = lc_printer.
*      ls_outputparams-getpdf   = lc_getpdf.
*      ls_outputparams-bumode   = lc_getpdf.
*      ls_outputparams-assemble = lc_assemble.
*
*      ls_control_parameters-no_open  = abap_true.
*      ls_control_parameters-no_close = abap_true.
*    ELSE.
*      ls_outputparams-preview = abap_false.
*      ls_outputparams-reqnew  = abap_true.
*    ENDIF.

    IF lv_function       IS NOT INITIAL
   AND lt_print_amstrg[] IS NOT INITIAL.

      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = ls_outputparams
        EXCEPTIONS
          cancel          = 1
          usage_error     = 2
          system_error    = 3
          internal_error  = 4
          OTHERS          = 5.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CALL FUNCTION lv_function
        EXPORTING
          /1bcdwb/docparams     = ls_params
          control_parameters    = ls_control_parameters
          it_ordem_carregamento = lt_print_amstrg
        EXCEPTIONS
          usage_error           = 1
          system_error          = 2
          internal_error        = 3
          OTHERS                = 4.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CALL FUNCTION 'FP_JOB_CLOSE'
        IMPORTING
          e_result       = ls_joboutput
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF iv_return = abap_true.
        CALL FUNCTION 'FP_GET_PDF_TABLE'
          IMPORTING
            e_pdf_table = lt_tfpcontent.

        IF lt_tfpcontent[] IS NOT INITIAL.
          APPEND VALUE #( stream_data = lt_tfpcontent[ 1 ] ) TO rv_pdf.
        ENDIF.

      ENDIF.

    ELSE.

      APPEND VALUE #( no_envelope = abap_true ) TO rv_pdf.

    ENDIF.

  ENDMETHOD.


  METHOD get_data.

    DATA: lt_ordfrete   TYPE STANDARD TABLE OF ty_ordfrete,
          lt_agrp_lacre TYPE STANDARD TABLE OF ty_agrp_lacre.

    DATA: lv_lacres  TYPE string,
          lv_created TYPE string.

    lt_ordfrete[] = it_ordem[].


    IF iv_docnum IS NOT INITIAL.
      SELECT db_key,
                 parent_key,
                 tor_id,
                 product_id,
                 maktx,
                 created_on,
                 bukrs,
                 namedistrib,
                 cnpjdistrib,
                 cnpjdistrib_text,
                 br_notafiscal,
                 transp,
                 cnpjtransp,
                 cnpjtransp_text,
                 namemotrst,
                 rgmotrst,
                 placa,
                 ct_seq,
                 rzsocirevend,
                 cnpjrevend,
                 cnpjrevend_text,
                 respreceb,
                 zz_sample_envelope,
                 zz_seal_number_1,
                 zz_seal_number_2,
                 zz_seal_number_3,
                 zz_seal_number_4,
                 zz_seal_number_5,
                 zz_seal_number_6,
                 zz_seal_number_7,
                 zz_seal_number_8,
                 NotaFiscal,
                 pnfullname
            FROM zi_tm_print_amostra
           WHERE br_notafiscal = @iv_docnum
            INTO TABLE @DATA(lt_print).
    ELSE.

      IF lt_ordfrete IS NOT INITIAL.

        SELECT db_key,
               parent_key,
               tor_id,
               product_id,
               maktx,
               created_on,
               bukrs,
               namedistrib,
               cnpjdistrib,
               cnpjdistrib_text,
               br_notafiscal,
               transp,
               cnpjtransp,
               cnpjtransp_text,
               namemotrst,
               rgmotrst,
               placa,
               ct_seq,
               rzsocirevend,
               cnpjrevend,
               cnpjrevend_text,
               respreceb,
               zz_sample_envelope,
               zz_seal_number_1,
               zz_seal_number_2,
               zz_seal_number_3,
               zz_seal_number_4,
               zz_seal_number_5,
               zz_seal_number_6,
               zz_seal_number_7,
               zz_seal_number_8,
               NotaFiscal,
               pnfullname
          FROM zi_tm_print_amostra
           FOR ALL ENTRIES IN @lt_ordfrete
         WHERE tor_id = @lt_ordfrete-tor_id
          INTO TABLE @lt_print.


      ENDIF.
    ENDIF.

    IF lt_print IS NOT INITIAL.

*      DATA(lt_print_aux) = lt_print[].
*      SORT lt_print_aux BY parent_key.
*      DELETE ADJACENT DUPLICATES FROM lt_print_aux COMPARING parent_key.
*
*      SELECT root_key,
*             seal_number
*        FROM zi_tm_ordem_carreg_lista_lacre
*         FOR ALL ENTRIES IN @lt_print_aux
*       WHERE root_key = @lt_print_aux-parent_key
*        INTO TABLE @DATA(lt_lacre).
*
*      IF sy-subrc IS INITIAL.
*
*        SORT lt_lacre BY root_key.
*
*        DATA(lt_lacre_head) = lt_lacre[].
*        SORT lt_lacre_head BY root_key.
*        DELETE ADJACENT DUPLICATES FROM lt_lacre_head COMPARING root_key.
*
*        LOOP AT lt_lacre_head ASSIGNING FIELD-SYMBOL(<fs_lacre_head>).
*
*          CLEAR lv_lacres.
*          READ TABLE lt_lacre TRANSPORTING NO FIELDS
*                                            WITH KEY root_key = <fs_lacre_head>-root_key
*                                            BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            LOOP AT lt_lacre ASSIGNING FIELD-SYMBOL(<fs_lacre>) FROM sy-tabix. "#EC CI_NESTED
*              IF <fs_lacre>-root_key NE <fs_lacre_head>-root_key.
*                EXIT.
*              ENDIF.
*
*              IF lv_lacres IS INITIAL.
*                lv_lacres = <fs_lacre>-seal_number.
*              ELSE.
*                lv_lacres = |{ lv_lacres },{ <fs_lacre>-seal_number }|.
*              ENDIF.
*            ENDLOOP.
*
*            lt_agrp_lacre = VALUE #( BASE lt_agrp_lacre ( root_key = <fs_lacre_head>-root_key
*                                                          lacres   = lv_lacres  ) ).
*          ENDIF.
*        ENDLOOP.
*
*        SORT lt_agrp_lacre BY root_key.

      LOOP AT lt_print ASSIGNING FIELD-SYMBOL(<fs_print>).
        CLEAR lv_lacres.
*          READ TABLE lt_agrp_lacre ASSIGNING FIELD-SYMBOL(<fs_agrp_lacres>)
*                                                 WITH KEY root_key = <fs_print>-parent_key
*                                                 BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            lv_lacres = <fs_agrp_lacres>-lacres.
*          ELSE.
*            CLEAR lv_lacres.
*          ENDIF.
*        <fs_print>-zz_seal_number_1 = |{ <fs_print>-zz_seal_number_1 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_2 = |{ <fs_print>-zz_seal_number_2 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_3 = |{ <fs_print>-zz_seal_number_3 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_4 = |{ <fs_print>-zz_seal_number_4 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_5 = |{ <fs_print>-zz_seal_number_5 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_6 = |{ <fs_print>-zz_seal_number_6 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_7 = |{ <fs_print>-zz_seal_number_7 ALPHA = OUT }|.
*        <fs_print>-zz_seal_number_8 = |{ <fs_print>-zz_seal_number_8 ALPHA = OUT }|.
        <fs_print>-BR_NotaFiscal    = |{ <fs_print>-BR_NotaFiscal ALPHA = OUT }|.

        IF <fs_print>-zz_seal_number_1 IS NOT INITIAL.
          lv_lacres = <fs_print>-zz_seal_number_1.
        ENDIF.
        IF <fs_print>-zz_seal_number_2 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_2.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_2.
          ENDIF.
        ENDIF.
        IF <fs_print>-zz_seal_number_3 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_3.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_3.
          ENDIF.
        ENDIF.
        IF <fs_print>-zz_seal_number_4 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_4.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_4.
          ENDIF.
        ENDIF.
        IF <fs_print>-zz_seal_number_5 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_5.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_5.
          ENDIF.
        ENDIF.
        IF <fs_print>-zz_seal_number_6 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_6.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_6.
          ENDIF.
        ENDIF.
        IF <fs_print>-zz_seal_number_7 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_7.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_7.
          ENDIF.
        ENDIF.
        IF <fs_print>-zz_seal_number_8 IS NOT INITIAL.
          IF lv_lacres IS INITIAL.
            lv_lacres = <fs_print>-zz_seal_number_8.
          ELSE.
            lv_lacres = lv_lacres && ',' && <fs_print>-zz_seal_number_8.
          ENDIF.
        ENDIF.

        lv_created = <fs_print>-created_on.

        rt_print_tab = VALUE #( BASE rt_print_tab ( parent_key     = <fs_print>-parent_key
                                                    product_id     = <fs_print>-product_id
                                                    maktx          = <fs_print>-maktx
                                                    created_on     = |{ lv_created+6(2) }.{ lv_created+4(2) }.{ lv_created(4) }|
                                                    bukrs          = <fs_print>-bukrs
                                                    lacres         = lv_lacres
                                                    name_distrib   = <fs_print>-namedistrib
                                                    cnpj_distrib   = <fs_print>-cnpjdistrib_text
                                                    br_notafiscal  = <fs_print>-br_notafiscal
                                                    transportador  = <fs_print>-transp
                                                    cnpj_transp    = <fs_print>-cnpjtransp_text
*                                                    name_motorista = <fs_print>-namemotrst
                                                    name_motorista = <fs_print>-pnfullname
                                                    rg_motorista   = <fs_print>-rgmotrst
                                                    placa          = <fs_print>-placa
                                                    compartimento  = <fs_print>-ct_seq
                                                    rzsoc_revend   = <fs_print>-rzsocirevend
                                                    cnpj_revend    = <fs_print>-cnpjrevend_text
                                                    resp_receb     = <fs_print>-respreceb
                                                    notafiscal     = <fs_print>-NotaFiscal
                                                    envelope       = <fs_print>-zz_sample_envelope ) ).

      ENDLOOP.
      SORT rt_print_tab BY compartimento.
*      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_function_name.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = gc_print_name
          IMPORTING
            e_funcname = rv_function.

      CATCH cx_fp_api_repository cx_fp_api_usage cx_fp_api_internal.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD call_form_danfe.

*    CONSTANTS: lc_modulo TYPE ztca_param_mod-modulo VALUE 'TM',
*               lc_chave1 TYPE ztca_param_par-chave1 VALUE 'ORDEM_CARREGAMENTO',
*               lc_chave2 TYPE ztca_param_par-chave2 VALUE 'IMPRESSAO_DOC',
*               lc_chave3 TYPE ztca_param_par-chave3 VALUE 'LOCAL_EXPEDICAO'.
*    DATA: lr_vstel TYPE RANGE OF vstel.
*
*    TRY.
*        NEW zclca_tabela_parametros( )->m_get_range(
*          EXPORTING
*            iv_modulo = lc_modulo
*            iv_chave1 = lc_chave1
*            iv_chave2 = lc_chave2
*            iv_chave3 = lc_chave3
*          IMPORTING
*            et_range  = lr_vstel ).
*
*        IF iv_vstel IN lr_vstel.

*      SELECT SINGLE OrdemFrete
*        FROM zi_tm_ordem_carregamento_of_nf
*        WHERE docnum = @iv_docnum
*        INTO @DATA(lv_orp_id).

*    IF sy-subrc IS INITIAL.
    me->call_form( it_ordem = VALUE #( ( ) ) iv_docnum = iv_docnum ).
*    ENDIF.

*        ENDIF.
*
*      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
*        RETURN.
*    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    me->gv_printer = iv_printer.
  ENDMETHOD.

ENDCLASS.
