CLASS zcltm_ve_corlacre DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .

    CONSTANTS: gc_modulo TYPE ze_param_modulo VALUE 'TM' ##NO_TEXT,
               gc_chave1 TYPE ze_param_chave1 VALUE 'APP SISPETRO' ##NO_TEXT,
               gc_chave2 TYPE ze_param_chave2 VALUE 'COR LACRE' ##NO_TEXT.

    DATA: go_parametros TYPE REF TO zclca_tabela_parametros,
          gv_cor_lacre  TYPE ze_param_low.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_parameters
      IMPORTING
        iv_chave3 TYPE ze_param_chave3.
ENDCLASS.



CLASS zcltm_ve_corlacre IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA: lt_data TYPE TABLE OF zi_tm_sispetro.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    MOVE-CORRESPONDING it_original_data TO lt_data.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      IF <fs_data>-Centro IS NOT INITIAL.
        get_parameters(
          EXPORTING
            iv_chave3 = CONV #( <fs_data>-Centro )
        ).
        IF sy-subrc IS INITIAL.
          <fs_data>-CorLacre = gv_cor_lacre.
        ENDIF.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING lt_data TO ct_calculated_data.

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    RETURN.
  ENDMETHOD.

  METHOD get_parameters.

    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2
            iv_chave3 = iv_chave3
          IMPORTING
            ev_param  = gv_cor_lacre
        ).

        IF gv_cor_lacre IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2
                                                       iv_chave3 = iv_chave3 ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
