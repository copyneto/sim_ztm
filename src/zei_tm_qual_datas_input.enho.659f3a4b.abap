"Name: \PR:SAPLSCMB0006\FO:MOVE_EXT_TO_INT_TMS_QUAL\SE:END\EI
ENHANCEMENT 0 ZEI_TM_QUAL_DATAS_INPUT.

  LOOP AT et_tmsres_qual ASSIGNING FIELD-SYMBOL(<fs_tmsres_qual>).
    READ TABLE it_bapi_tmsres_qual ASSIGNING FIELD-SYMBOL(<fs_bapi_tmsres_qual>) "#EC CI_STDSEQ
      WITH KEY resource = <fs_tmsres_qual>-name
               qualitype = <fs_tmsres_qual>-qualtype
               jurisdic = <fs_tmsres_qual>-jurisdic
               qualivalue = <fs_tmsres_qual>-qualification.
    IF sy-subrc = 0.
      <fs_tmsres_qual>-valid_from = <fs_bapi_tmsres_qual>-valid_from.
      <fs_tmsres_qual>-valid_to = <fs_bapi_tmsres_qual>-valid_to.
    ENDIF.
  ENDLOOP.
ENDENHANCEMENT.
