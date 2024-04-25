CLASS zcltm_comb_recur DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS upload_file
      IMPORTING
        !iv_file     TYPE xstring
        !iv_filename TYPE string
      EXPORTING
        !et_return   TYPE bapiret2_t .

    CONSTANTS: gc_msgtype_e TYPE bapiret2-type VALUE 'E' ##NO_TEXT,
               gc_msgtype_s TYPE bapiret2-type VALUE 'S' ##NO_TEXT,
               gc_id        TYPE bapiret2-id VALUE 'ZCA_EXCEL' ##NO_TEXT,
               gc_number    TYPE bapiret2-number VALUE '000' ##NO_TEXT.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcltm_comb_recur IMPLEMENTATION.
  METHOD upload_file.
    DATA: lt_file TYPE TABLE OF zstm_comb_recur.
    DATA: lt_comb_recur TYPE TABLE OF zttm_comb_recur.
    DATA: ls_comb_recur TYPE zstm_comb_recur.
* ---------------------------------------------------------------------------
* Converte arquivo excel para tabela
* ---------------------------------------------------------------------------
    DATA(lo_excel) = NEW zclca_excel( iv_filename = iv_filename
                                      iv_file     = iv_file ).
    "lo_excel->gv_quant = abap_true.
    lo_excel->get_sheet( IMPORTING et_return = DATA(lt_return)              " Ignorar validação durante carga
                         CHANGING  ct_table  = lt_file[] ).

    IF line_exists( lt_return[ type = gc_msgtype_e ] ).           "#EC CI_STDSEQ
      et_return = lt_return.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara dados para salvar
* ---------------------------------------------------------------------------
    DELETE lt_file WHERE combination_resource_id  IS INITIAL. "#EC CI_STDSEQ

    lt_comb_recur = VALUE #( FOR ls_file IN lt_file ( mandt = sy-mandt
                                                      combination_resource_id = ls_file-combination_resource_id
                                                      seq_num = ls_file-seq_num
                                                      equi_type = ls_file-equi_type
                                                      equi_code = ls_file-equi_code
                                                      resource_id = ls_file-resource_id ) ).

    IF lt_comb_recur IS NOT INITIAL.
      MODIFY zttm_comb_recur FROM TABLE lt_comb_recur.

      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
        et_return[] = VALUE #( BASE et_return ( type = gc_msgtype_s id = gc_id number = gc_number ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
