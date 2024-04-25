CLASS zcltm_romaneio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_filters,
             Docnum         TYPE RANGE OF j_1bdocnum,
             Emissao        TYPE RANGE OF j_1bdocdat,
             Transportadora TYPE RANGE OF name1_gp,
           END OF ty_filters.

    DATA: gs_filters    TYPE ty_filters.

    METHODS build
      IMPORTING
                it_filters    TYPE  if_rap_query_filter=>tt_name_range_pairs
      RETURNING VALUE(rv_pdf) TYPE ze_stream_romaneio.

    METHODS get_data
      EXPORTING
        es_header TYPE zstm_minuta_faturamento
        et_item   TYPE zctgtm_minuta_faturamento_item.

    METHODS get_filters
      IMPORTING it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    METHODS get_smartform
      IMPORTING
                is_header          TYPE zstm_minuta_faturamento
                it_item            TYPE zctgtm_minuta_faturamento_item
      RETURNING VALUE(rv_pdf_file) TYPE xstring.

    METHODS call_form
      IMPORTING
        is_header     TYPE zstm_minuta_faturamento
        it_item       TYPE zctgtm_minuta_faturamento_item
      RETURNING
        VALUE(rt_otf) TYPE tsfotf .

    "! Busca função
    "! @parameter rv_func | Retorna a função
    METHODS get_function
      RETURNING
        VALUE(rv_func) TYPE rs38l_fnam .

    "! Converte OTF para envio por email
    "! @parameter it_otf | Converte otf
    METHODS convert_otf
      IMPORTING
                 it_otf      TYPE tt_itcoo
      EXPORTING  ev_pdf_file TYPE xstring
      EXCEPTIONS not_convert_otf .

    METHODS get_adobeform
      IMPORTING
                is_header            TYPE zstm_minuta_faturamento
      RETURNING VALUE(rt_formoutput) TYPE tfpcontent.

ENDCLASS.



CLASS zcltm_romaneio IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA: lt_tab                   TYPE TABLE OF zi_tm_impressao_de_romaneio_ce.
    DATA: lt_master_keys           TYPE cl_somu_form_services=>ty_gt_key.
    DATA: lv_content               TYPE  xstring.
    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services,
          lt_keys                  TYPE cl_somu_form_services=>ty_gt_key.


    TRY.
        "Requested data
        IF io_request->is_data_requested(  ).

          "Paginacao
          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                      THEN 0 ELSE lv_page_size )  .

          "Recupera filtros
          TRY.
              TRY.
                  DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK
                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.
              "Busca os parametros da custom entity
*              DATA(lv_salesorder)     =   VALUE #( lt_parameters[ parameter_name =  'P_HANDLINGUNITEXTERNALID' ]-value OPTIONAL ).
              "Cria instancia

              lt_tab = VALUE #( ( stream_data = me->build( it_filters = lt_filters ) ) ).

              io_response->set_total_number_of_records( 1 ).

*  " -------------- Send the response back to UI------------
              io_response->set_data( lt_tab ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.


        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.

  ENDMETHOD.

  METHOD build.
    DATA lt_formoutput TYPE tfpcontent.

    me->get_filters( it_filters ).

    "CHECK gs_parameters IS NOT INITIAL.

    me->get_data(
      IMPORTING
        es_header     = DATA(ls_header)
        et_item       = DATA(lt_item)
    ).


    "CHECK ls_header IS NOT INITIAL AND lt_item[] IS NOT INITIAL.
    lt_formoutput = me->get_adobeform( is_header = ls_header ).

    CHECK lt_formoutput IS NOT INITIAL.

    rv_pdf = lt_formoutput[ 1 ].
*   rv_pdf = me->get_smartform( is_header = ls_header
*                               it_item   = lt_item ).

  ENDMETHOD.

  METHOD get_data.

    DATA: lv_total_vol      TYPE j_1bcte_vol_transp,
          lv_total_peso     TYPE ntgew_15,
          lv_numero_Seq     TYPE char10,
          lv_transportadora TYPE char100.

*     Seleciona os itens da tabela
    SELECT docnum,
           Nota,
           Emissao,
           vol,
           Pedido,
           Peso,
           Valor,
           Cliente,
           Cidade,
           uf,
           UnidadePeso,
           vol_unit,
           Moeda,
           Transportadora,
           TransportadoraDesc
        FROM zi_tm_impressao_de_romaneio
        INTO TABLE @DATA(lt_itens)
        WHERE docnum IN @gs_filters-docnum
          AND Emissao IN @gs_filters-emissao.

    LOOP AT lt_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).
      lv_total_vol = <fs_itens>-vol + lv_total_vol.
      lv_total_peso = <fs_itens>-Peso + lv_total_peso.
    ENDLOOP.

    et_item = CORRESPONDING #( lt_itens ).

    IF lt_itens IS NOT INITIAL.
      lv_transportadora = lt_itens[ 1 ]-TransportadoraDesc.
    ENDIF.


    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'Z_MINUTA_F'
      IMPORTING
        number                  = lv_numero_Seq
*       quantity                =
*       returncode              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    es_header = VALUE #(

     titulo02       = TEXT-t00
     carregamento   = lv_numero_Seq
     emissao        = sy-datum
     usuario        = sy-uname
     transportadora = lv_transportadora
     item           = et_item
     total_vol      = lv_total_vol
     total_peso     = lv_total_peso
     total_nfe      = Lines( et_item ) ).

  ENDMETHOD.

  METHOD get_filters.
    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).

      CASE <fs_filters>-name.
        WHEN 'DOCNUM'.
          gs_filters-Docnum = CORRESPONDING #( <fs_filters>-range ).

        WHEN 'EMISSAO'.
          gs_filters-Emissao = CORRESPONDING #( <fs_filters>-range ).

        WHEN 'TRANSPORTADORA'.
          gs_filters-Transportadora = CORRESPONDING #( <fs_filters>-range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_smartform.

    DATA: lo_fp     TYPE REF TO if_fp,
          lo_pdfobj TYPE REF TO if_fp_pdf_object,
          ls_meta   TYPE sfpmetadata.

    DATA(lt_otf) = me->call_form( is_header = is_header
                                  it_item   = it_item ).

    convert_otf( EXPORTING  it_otf          = lt_otf
                 IMPORTING  ev_pdf_file     = rv_pdf_file
                 EXCEPTIONS not_convert_otf = 2
                 OTHERS                     = 3 ).

  ENDMETHOD.

  METHOD call_form.

    CONSTANTS lc_destino TYPE rspopname VALUE 'LP01'.

    DATA: ls_ctrlop TYPE ssfctrlop,
          ls_compop TYPE ssfcompop,
          ls_otf    TYPE ssfcrescl.

    DATA(lv_function) = get_function( ).

    ls_ctrlop-getotf    = abap_true.
    ls_ctrlop-no_dialog = abap_true.

    ls_compop-tdnoprev  = abap_true.
    ls_compop-tddest    = lc_destino.
    ls_compop-tdnewid   = abap_true.

    CALL FUNCTION lv_function
      EXPORTING
        control_parameters = ls_ctrlop
        output_options     = ls_compop
        user_settings      = abap_false
        gs_header          = is_header
        gt_lista           = it_item
      IMPORTING
        job_output_info    = ls_otf
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc = 0.
      rt_otf = ls_otf-otfdata[].                          "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.

  METHOD convert_otf.
    CONSTANTS lc_format TYPE c LENGTH 3 VALUE 'PDF'.
    DATA lt_pdf                TYPE tlinet.
    DATA lv_pdf_filesize       TYPE i.

    CLEAR: ev_pdf_file.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = lc_format
      IMPORTING
        bin_filesize          = lv_pdf_filesize
        bin_file              = ev_pdf_file
      TABLES
        otf                   = it_otf[]
        lines                 = lt_pdf[]
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc NE 0.
      RAISE not_convert_otf.
    ENDIF.

  ENDMETHOD.

  METHOD get_function.

    CONSTANTS lc_form TYPE tdsfname VALUE 'ZSFTM_MINUTA_DE_FATURAMENTO'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = lc_form
      IMPORTING
        fm_name            = rv_func
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD get_adobeform.

    DATA: lv_fm_name            TYPE rs38l_fnam,
          ls_fp_docparams       TYPE sfpdocparams,
          ls_fp_outputparams    TYPE sfpoutputparams,
          ls_control_parameters TYPE  ssfctrlop,
          lv_lines              TYPE i
          .

    DATA: lo_pdf_merger TYPE REF TO cl_rspo_pdf_merge.

    ls_fp_outputparams-dest     = 'LP01'.
    ls_fp_outputparams-device   = 'PRINTER'.
    ls_fp_outputparams-nodialog = abap_true.
    ls_fp_outputparams-getpdf   = 'M'.
    ls_fp_outputparams-bumode   = 'M'.
    ls_fp_outputparams-assemble = 'S'.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_fp_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    TRY.
*&---- Get the name of the generated function module
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME' ##FM_SUBRC_OK
          EXPORTING
            i_name     = 'ZAFTM_MINUTA_FATURAMENTO'
          IMPORTING
            e_funcname = lv_fm_name.
      CATCH cx_fp_api_internal .
        RETURN.
      CATCH cx_fp_api_repository  .
        RETURN.
      CATCH cx_fp_api_usage .
        RETURN.
    ENDTRY.

    ls_control_parameters-no_open  = abap_true.
    ls_control_parameters-no_close = abap_true.

*&--- Call the generated function module
    CALL FUNCTION lv_fm_name
      EXPORTING
        /1bcdwb/docparams  = ls_fp_docparams
        control_parameters = ls_control_parameters
        gs_dados           = is_header
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

*&---- Close the spool job
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    CALL FUNCTION 'FP_GET_PDF_TABLE'
      IMPORTING
        e_pdf_table = rt_formoutput.

  ENDMETHOD.

ENDCLASS.
