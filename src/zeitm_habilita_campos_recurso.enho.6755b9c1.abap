"Name: \TY:/SCMB/CL_TMSRES_ALV_QUAL\ME:SET_DEFAULT_FIELDCAT\SE:END\EI
ENHANCEMENT 0 ZEITM_HABILITA_CAMPOS_RECURSO.

  LOOP AT alv_fieldcat REFERENCE INTO DATA(ls_r_fieldcat).

    CASE ls_r_fieldcat->fieldname.

      WHEN 'VALID_FROM' OR 'VALID_TO'.
        ls_r_fieldcat->no_out     = /scmb/if_tmsres_alv~false.
        ls_r_fieldcat->tech       = /scmb/if_tmsres_alv~false.

      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDENHANCEMENT.
