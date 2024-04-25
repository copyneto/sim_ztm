CLASS zcltm_determinacao_iva DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS upload_file
      IMPORTING
        !iv_file     TYPE xstring
        !iv_filename TYPE string
      EXPORTING
        !et_return   TYPE bapiret2_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcltm_determinacao_iva IMPLEMENTATION.
  METHOD constructor.
    RETURN.
  ENDMETHOD.

  METHOD upload_file.

*    RETURN.
    DATA: lt_file     TYPE TABLE OF zstm_determ_iva_file.
    DATA: lt_det_iva  TYPE TABLE OF zttm_determ_iva.
    DATA: ls_det_file TYPE zstm_determ_iva_file.
    DATA: lv_message  TYPE string.
* ---------------------------------------------------------------------------
* Converte arquivo excel para tabela
* ---------------------------------------------------------------------------
    DATA(lo_excel) = NEW zclca_excel( iv_filename = iv_filename
                                      iv_file     = iv_file ).
    "lo_excel->gv_quant = abap_true.
    lo_excel->get_sheet( IMPORTING et_return = DATA(lt_return)              " Ignorar validação durante carga
                         CHANGING  ct_table  = lt_file[] ).

    IF line_exists( lt_return[ type = 'E' ] ).           "#EC CI_STDSEQ
      et_return = lt_return.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara dados para salvar
* ---------------------------------------------------------------------------
    DELETE lt_file WHERE cenario  IS INITIAL
                     OR uforigem  IS INITIAL
                     OR ufdestino IS INITIAL.            "#EC CI_STDSEQ

    IF lt_file IS NOT INITIAL.

      lt_det_iva = VALUE #( FOR ls_file IN lt_file ( mandt          = sy-mandt
                                                     cenario        = |{ ls_file-cenario ALPHA = IN }|      " CHANGE - JWSILVA - 08.04.2024
                                                     uforigem       = ls_file-uforigem
                                                     ufdestino      = ls_file-ufdestino
                                                     uftransp       = ls_file-uftransp
                                                     transportadora = ls_file-transportadora
                                                     clientefornec  = ls_file-clientefornec
                                                     iva            = ls_file-iva ) ).

      IF lt_det_iva IS NOT INITIAL.

        MODIFY zttm_determ_iva FROM TABLE lt_det_iva.

        IF sy-subrc IS INITIAL.

          MESSAGE s000(ztm_determinacao_iva) INTO lv_message.
          et_return[] = VALUE #( BASE et_return ( type = 'S' id = 'ZTM_DETERMINACAO_IVA' number = '000' message = lv_message ) ).

          COMMIT WORK AND WAIT.

        ENDIF.

      ENDIF.

    ELSE. " Erro

      MESSAGE e001(ztm_determinacao_iva) INTO lv_message.
      et_return[] = VALUE #( BASE et_return ( type = 'S' id = 'ZTM_DETERMINACAO_IVA' number = '001' message = lv_message ) ).

    ENDIF.


  ENDMETHOD.

ENDCLASS.
