"Name: \TY:/SCMB/CL_TMSRES_ALV_QUAL\IN:/SCMB/IF_TMSRES_ALV\ME:SET_INPUT_FIELDS\SE:END\EI
ENHANCEMENT 0 ZEITM_HABILITA_CAMPOS_RECURSO.
  CALL METHOD change_fieldcat_edit
    EXPORTING
      iv_field = 'VALID_FROM'
      iv_edit  = lv_edit.
  CALL METHOD change_fieldcat_edit
    EXPORTING
      iv_field = 'VALID_TO'
      iv_edit  = lv_edit.
ENDENHANCEMENT.
