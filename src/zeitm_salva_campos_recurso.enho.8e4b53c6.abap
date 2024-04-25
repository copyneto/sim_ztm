"Name: \FU:/SCMB/TMS_RES_ALV2DB_QUAL\SE:END\EI
ENHANCEMENT 0 ZEITM_SALVA_CAMPOS_RECURSO.

  CONVERT DATE is_data-valid_from INTO TIME STAMP es_data-valid_from TIME ZONE 'UTC'.
  CONVERT DATE is_data-valid_to INTO TIME STAMP es_data-valid_to TIME ZONE 'UTC'.

*  es_data-valid_from = CONV char08( is_data-valid_from ).
*  es_data-valid_to   = CONV char08( is_data-valid_to ).

ENDENHANCEMENT.
