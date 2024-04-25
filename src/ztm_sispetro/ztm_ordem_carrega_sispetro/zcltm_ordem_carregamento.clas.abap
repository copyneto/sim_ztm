CLASS zcltm_ordem_carregamento DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_printer TYPE rspopname OPTIONAL.

    METHODS call_form
      IMPORTING
        !iv_tor_id    TYPE /scmtms/tor_id
        !iv_return    TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_pdf) TYPE ze_stream_ordem_carr .

    METHODS call_form_danfe
      IMPORTING
        !iv_docnum TYPE j_1bdocnum
        !iv_vstel  TYPE vstel.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_data
      IMPORTING
        !iv_tor_id                   TYPE /scmtms/tor_id
      RETURNING
        VALUE(rs_ordem_carregamento) TYPE zstm_ordem_carregamento .
    METHODS get_function_name
      RETURNING
        VALUE(rv_function) TYPE funcname .

    DATA gv_printer TYPE rspopname.
ENDCLASS.



CLASS zcltm_ordem_carregamento IMPLEMENTATION.


  METHOD call_form.

    CONSTANTS: lc_tddest   TYPE rspopname  VALUE 'LP01',
               lc_printer  TYPE fpmedium   VALUE 'PRINTER',
               lc_getpdf   TYPE fpgetpdf   VALUE 'M',
               lc_bumode   TYPE fpbumode   VALUE 'M',
               lc_assemble TYPE fpassemble VALUE 'S'.

    DATA: ls_outputparams       TYPE sfpoutputparams,
          ls_params             TYPE sfpdocparams,
          ls_joboutput          TYPE sfpjoboutput,
          ls_control_parameters TYPE ssfctrlop.

    DATA: lt_tfpcontent TYPE tfpcontent.

    DATA(lv_function) = me->get_function_name( ).
    DATA(ls_ordem_carregamento) = me->get_data( EXPORTING iv_tor_id = iv_tor_id ).

    DATA: lo_pdf_merger TYPE REF TO cl_rspo_pdf_merge.

    IF me->gv_printer IS NOT INITIAL.
      ls_outputparams-dest     = me->gv_printer.
      ls_outputparams-reqimm   = abap_true.
    ELSE.
      ls_outputparams-dest     = lc_tddest.
      ls_outputparams-getpdf   = lc_getpdf.
      ls_outputparams-bumode   = lc_bumode.
      ls_outputparams-assemble = lc_assemble.

      ls_control_parameters-no_open  = abap_true.
      ls_control_parameters-no_close = abap_true.
    ENDIF.

    ls_outputparams-nodialog = abap_true.
    ls_outputparams-device   = lc_printer.

*    IF iv_return = abap_true.
*      ls_outputparams-device   = lc_printer.
*      ls_outputparams-getpdf   = lc_getpdf.
*      ls_outputparams-bumode   = lc_bumode.
*      ls_outputparams-assemble = lc_assemble.
*
*      ls_control_parameters-no_open  = abap_true.
*      ls_control_parameters-no_close = abap_true.
*    ELSE.
*      ls_outputparams-preview = abap_false.
*      ls_outputparams-reqnew = abap_true.
*    ENDIF.


    IF lv_function IS NOT INITIAL AND
      ls_ordem_carregamento IS NOT INITIAL.

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
        return.
      ENDIF.

      CALL FUNCTION lv_function
        EXPORTING
          /1bcdwb/docparams     = ls_params
          control_parameters    = ls_control_parameters
          is_ordem_carregamento = ls_ordem_carregamento
        EXCEPTIONS
          usage_error           = 1
          system_error          = 2
          internal_error        = 3
          OTHERS                = 4.

      IF sy-subrc <> 0.
        return.
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
        return.
      ENDIF.

      IF iv_return = abap_true.
        CALL FUNCTION 'FP_GET_PDF_TABLE'
          IMPORTING
            e_pdf_table = lt_tfpcontent.

        IF lt_tfpcontent[] IS NOT INITIAL.
          rv_pdf = lt_tfpcontent[ 1 ].
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD call_form_danfe.

    CONSTANTS: lc_modulo TYPE ztca_param_mod-modulo VALUE 'TM',
               lc_chave1 TYPE ztca_param_par-chave1 VALUE 'ORDEM_CARREGAMENTO',
               lc_chave2 TYPE ztca_param_par-chave2 VALUE 'IMPRESSAO_DOC',
               lc_chave3 TYPE ztca_param_par-chave3 VALUE 'LOCAL_EXPEDICAO'.

    DATA: lr_vstel TYPE RANGE OF vstel.

    TRY.
        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo
            iv_chave1 = lc_chave1
            iv_chave2 = lc_chave2
            iv_chave3 = lc_chave3
          IMPORTING
            et_range  = lr_vstel ).

        IF NOT iv_vstel IN lr_vstel.

          SELECT SINGLE OrdemFrete
            FROM zi_tm_ordem_carregamento_of_nf
            WHERE docnum = @iv_docnum
            INTO @DATA(lv_orp_id).

          IF sy-subrc IS INITIAL.
            me->call_form( iv_tor_id = lv_orp_id ).
          ENDIF.

        ENDIF.

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_data.

    CONSTANTS: gc_mask_cnpj TYPE string VALUE '__.___.___/____-__',
               gc_mask_cpf  TYPE string VALUE '___.___.___-__'.

    DATA: lv_cpf_cnpj_for TYPE c LENGTH 18.
    DATA: lv_cnpj_tra TYPE c LENGTH 18.

    SELECT SINGLE
      Orp_Id,
      DtHr_Emissao,
      Incoterms,
      BaseText,
      LocalText,
      Fornecedor,
      CpfCnpjFornecedor,
      NomeFornecedor,
      BpTransportador,
      CnpjTransportador,
      Transportador,
      Placa1,
      Placa2,
      Placa3,
      Placa4,
      TotalLacre,
      CorLacre,
      BpMotorista,
      Motorista,
      DocMotorista,
      UnidadeVolume,
      Volume
      FROM zi_tm_ordem_carregamento
      WHERE Orp_Id = @iv_tor_id
      INTO @DATA(ls_header).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT
      db_key,
      parent_key,
      Orp_Id,
      zz_fu_db_key,
      ct_seq,
      zz_fu_max_util,
      item_descr,
      gro_vol_uni,
      gro_vol_val,
      res_id,
      envelope
      FROM zi_tm_ordem_carregamento_item
      WHERE Orp_Id = @iv_tor_id
      INTO TABLE @DATA(lt_itens).

    SELECT
      ColetaAmostra
      FROM zi_tm_ordem_carregamento_col
      WHERE Orp_Id = @iv_tor_id
      ORDER BY Orp_Id
      INTO TABLE @DATA(lt_coleta).

    SELECT Nfe
      FROM zi_tm_ordem_carregamento_nf
      WHERE Orp_Id = @iv_tor_id
      ORDER BY Orp_Id
      INTO TABLE @DATA(lt_nf).

    SELECT
      item_descr,
      UnidadeMedida,
      gro_vol_val,
      BillOfMaterialItemUnit,
      BillOfMaterialItemQuantity,
      ComponentDescription,
      bklas,
      Centro
      FROM zi_tm_ordem_carregamento_obs
      WHERE Orp_Id = @iv_tor_id
      INTO TABLE @DATA(lt_obs).

    SELECT
      seal_number
      FROM zi_tm_ordem_carregamento_lis_l
      WHERE Orp_Id = @iv_tor_id
      ORDER BY Orp_Id
      INTO TABLE @DATA(lt_lacre).

    SELECT _t001w~name1 AS visto_empresa, _Loct~descr40 AS visto_loc "#EC CI_BUFFJOIN
    FROM /scmtms/d_torrot AS _TorRoot
    INNER JOIN /scmtms/d_torite AS _TorItem
    ON  _TorItem~parent_key = _TorRoot~db_key
    AND _TorItem~item_cat = 'PRD'
    LEFT JOIN t001w AS _t001w                          "#EC CI_BUFFJOIN
    ON _t001w~werks = _TorItem~erp_plant_id
    LEFT JOIN /sapapo/loc AS _Loc
    ON _Loc~locno = _TorItem~src_loc_idtrq
    LEFT JOIN /sapapo/loct AS _Loct                    "#EC CI_BUFFJOIN
    ON _Loct~locid = _Loc~locid
    AND _Loct~spras = @sy-langu
    WHERE _TorRoot~tor_id = @iv_tor_id
    INTO @DATA(ls_visto)
    UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc = 0.
      DATA(lv_ok) = abap_true.
    ENDIF.

    CONCATENATE LINES OF lt_coleta INTO DATA(lv_coleta) SEPARATED BY ', '.

    CONCATENATE LINES OF lt_nf INTO DATA(lv_nf) SEPARATED BY ' - '.

    "CONCATENATE LINES OF lt_obs INTO DATA(lv_obs) SEPARATED BY space.

    CONCATENATE LINES OF lt_lacre INTO DATA(lv_lacre) SEPARATED BY ', '.

    IF  strlen( ls_header-CpfCnpjFornecedor ) > 11.
      WRITE ls_header-CpfCnpjFornecedor USING EDIT MASK gc_mask_cnpj TO lv_cpf_cnpj_for.
    ELSE.
      WRITE ls_header-CpfCnpjFornecedor USING EDIT MASK gc_mask_cpf TO lv_cpf_cnpj_for.
    ENDIF.

    WRITE ls_header-cnpjtransportador USING EDIT MASK gc_mask_cnpj TO lv_cnpj_tra.


    rs_ordem_carregamento-orp_nro = ls_header-orp_id.
    rs_ordem_carregamento-emissao_hora = ls_header-dthr_emissao.
    rs_ordem_carregamento-incoterms = ls_header-incoterms.
    rs_ordem_carregamento-base = ls_header-basetext.
    rs_ordem_carregamento-local = ls_header-localtext.
    rs_ordem_carregamento-cpf_cnpj = lv_cpf_cnpj_for.
    rs_ordem_carregamento-desc_cnpj = ls_header-nomefornecedor.
    rs_ordem_carregamento-transportador = |{ lv_cnpj_tra } { ls_header-transportador } |.
    rs_ordem_carregamento-placa1 = ls_header-placa1.
    rs_ordem_carregamento-placa2 = ls_header-placa2.
    rs_ordem_carregamento-placa3 = ls_header-placa3.
    rs_ordem_carregamento-placa4 = ls_header-placa4.
    rs_ordem_carregamento-itens = CORRESPONDING #( lt_itens ).
*    rs_ordem_carregamento-qtde_lacres = ls_header-totallacre.
    rs_ordem_carregamento-qtde_lacres = lines( lt_lacre ).
    rs_ordem_carregamento-cor_lacres = ls_header-corlacre.
    rs_ordem_carregamento-volume = ls_header-volume.
    rs_ordem_carregamento-unit_volume = ls_header-unidadevolume.
    rs_ordem_carregamento-lacres = lv_lacre.
    rs_ordem_carregamento-motorista = ls_header-motorista.
    rs_ordem_carregamento-doc_motorista = ls_header-docmotorista.


    DATA: lv_format TYPE p DECIMALS 0.

    DATA(lv_lines_obs) = lines( lt_obs ).

    LOOP AT lt_obs ASSIGNING FIELD-SYMBOL(<fs_obs>).
      IF sy-tabix = 1.
        rs_ordem_carregamento-observacoes = |Observações: | ##NO_TEXT.
      ENDIF.
      lv_format = <fs_obs>-gro_vol_val.
      ##NO_TEXT
      rs_ordem_carregamento-observacoes = | { rs_ordem_carregamento-observacoes } { <fs_obs>-item_descr } { lv_format } = { <fs_obs>-billofmaterialitemquantity } { <fs_obs>-componentdescription }|.
      IF sy-tabix <> lv_lines_obs.
        rs_ordem_carregamento-observacoes = rs_ordem_carregamento-observacoes && ` || `.
      ENDIF.
    ENDLOOP.

    CONDENSE rs_ordem_carregamento-observacoes.

    ##NO_TEXT
    rs_ordem_carregamento-notas_fiscais = |Nota(s) Fiscal(is): { lv_nf }|.
    rs_ordem_carregamento-coleta = lv_coleta.


    rs_ordem_carregamento-visto_empresa = ls_visto-visto_empresa.
    rs_ordem_carregamento-visto_loc = ls_visto-visto_loc.
  ENDMETHOD.


  METHOD get_function_name.

    CONSTANTS lc_name TYPE fpname VALUE 'ZAFTM_ORDEM_CARREGAMENTO'.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = lc_name
          IMPORTING
            e_funcname = rv_function.

      CATCH cx_fp_api_repository cx_fp_api_usage cx_fp_api_internal.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD constructor.
    me->gv_printer = iv_printer.
  ENDMETHOD.

ENDCLASS.
