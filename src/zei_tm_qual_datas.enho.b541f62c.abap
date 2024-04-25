"Name: \PR:/SCMB/SAPLTMSRES_SERVICE\FO:CONVERT_QUAL_TO_DB\SE:END\EI
ENHANCEMENT 0 ZEI_TM_QUAL_DATAS.
*
  es_restmssk-valid_from = COND #(
   WHEN is_qual-valid_from IS NOT INITIAL
   THEN is_qual-valid_from && '000000'
   ELSE es_restmssk-valid_from
  ).
  es_restmssk-valid_to = COND #(
   WHEN is_qual-valid_to IS NOT INITIAL
   THEN is_qual-valid_to && '000000'
   ELSE es_restmssk-valid_to
  ).
ENDENHANCEMENT.
