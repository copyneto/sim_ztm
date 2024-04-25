*&---------------------------------------------------------------------*
*& Report ZTMR_SISPETRO_SEND_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztmr_sispetro_send_data.

DATA: lt_key TYPE /bobf/t_frw_key.

PARAMETERS: p_data TYPE sy-datum DEFAULT sy-datum.

START-OF-SELECTION.

  SELECT db_key AS key                                  "#EC CI_SEL_DEL
    FROM zi_tm_sispetro AS sispetro
    INNER JOIN zi_tm_sispetro_nota_fiscal AS _SispetroNF
    ON sispetro~db_key = _SispetroNF~parent_key
    INNER JOIN j_1bnfe_active AS active  ON active~docnum = _SispetroNF~br_notafiscal
    LEFT OUTER JOIN zttm_logsispetr0 as _SispetroLog
    ON _SispetroLog~id_processo = sispetro~db_key
    WHERE active~docnum IS NOT NULL
      AND active~docsta EQ 1
      AND active~authdate = @p_data
      AND _SispetroLog~id_processo IS NULL
    GROUP BY db_key
     INTO TABLE @lt_key.

  CHECK lt_key IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_key.

  DATA(lt_return) = NEW zcltm_sispetro_send_data( )->exec( it_key = lt_key ).
