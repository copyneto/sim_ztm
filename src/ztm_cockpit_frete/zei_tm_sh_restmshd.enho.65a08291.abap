"Name: \FU:/SAPAPO/OO_SH_CM_PROFILE\SE:END\EI
ENHANCEMENT 0 ZEI_TM_SH_RESTMSHD.
      IF callcontrol-step = 'DISP'.
    IF NOT ls_interface-value IS INITIAL.
      lv_cmprf = ls_interface-value.
      REPLACE ALL OCCURRENCES OF '*' IN lv_cmprf WITH '%'.
      REPLACE ALL OCCURRENCES OF '+' IN lv_cmprf WITH '_'.
      SELECT * FROM /sapapo/cmprf
          INTO TABLE lt_cmprf
          WHERE cmprofile LIKE lv_cmprf.
      IF sy-subrc = 0.
      ENDIF.
    ELSE.
      SELECT * FROM /sapapo/cmprf
                INTO TABLE lt_cmprf.
    ENDIF.
    IF NOT lt_cmprf IS INITIAL.
      refresh record_tab.
      LOOP AT lt_cmprf INTO ls_cmprf. "#EC CI_LOOP_INTO_WA
         DATA(lv_zz_seal_number_total) = conv char3( ls_cmprf-ZZ_SEAL_NUMBER_TOTAL ).
         CONDENSE lv_zz_seal_number_total NO-GAPS.
         record_tab-string = ls_cmprf-mandt.
         record_tab-string+3(10) = ls_cmprf-cmprofile(10).
         record_tab-string+13(2) = ls_cmprf-SEQUENCE(2).
         record_tab-string+15(10) = ls_cmprf-CTYPE(10).
         record_tab-string+25(10) = ls_cmprf-TEXT(10).
         record_tab-string+35(3) = lv_zz_seal_number_total(3).
*         CONCATENATE ls_cmprf-mandt
*                     ls_cmprf-cmprofile(10)
*                     ls_cmprf-SEQUENCE(2)
*                     ls_cmprf-CTYPE(10)
*                     ls_cmprf-TEXT(10)
*                     lv_zz_seal_number_total(3)
*             INTO record_tab-string RESPECTING BLANKS.
        APPEND record_tab.
      ENDLOOP.
    ENDIF.
    ENDIF.
ENDENHANCEMENT.
