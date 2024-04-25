FUNCTION zfmtm_edoc_br_flex.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_HEADERDATA) TYPE  BAPI_INCINV_CREATE_HEADER
*"     VALUE(IS_EXPORT) TYPE  ZSTM_EXPORT_FLEX
*"  TABLES
*"      TT_TM_ITEMDATA STRUCTURE  BAPI_INCINV_CREATE_TM_ITEM
*"      TT_RETURN STRUCTURE  BAPIRET2
*"  CHANGING
*"     VALUE(CV_INVOICEDOCNUMBER) TYPE  BAPI_INCINV_FLD-INV_DOC_NO
*"     VALUE(CV_FISCALYEAR) TYPE  BAPI_INCINV_FLD-FISC_YEAR
*"----------------------------------------------------------------------

  DATA: lt_item_data TYPE TABLE OF  bapi_incinv_create_item.
  DATA: ls_export     TYPE zstm_export_flex.
  DATA: ls_export2    TYPE zstm_export_flex.
  DATA: lv_id     TYPE indx_srtfd.
  DATA: ls_indx TYPE indx.


*Export Memory - CL_LJ1BIF02-ZITM_EDOC_BR_FLEX
  ls_export = is_export.

  SHIFT is_headerdata-ref_doc_no LEFT DELETING LEADING '0'.
  lv_id = |{ is_headerdata-comp_code }| & |{ is_headerdata-ref_doc_no }|.

  DATA(lv_data) = sy-datum.
  DATA(lv_mandt) = sy-mandt.
  EXPORT ls_export TO  DATABASE indx(zz) CLIENT lv_mandt ID lv_id.


  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      headerdata       = is_headerdata
    IMPORTING
      invoicedocnumber = cv_invoicedocnumber
      fiscalyear       = cv_fiscalyear
    TABLES
      itemdata         = lt_item_data
      return           = tt_return
      tm_itemdata      = tt_tm_itemdata.

  IF cv_invoicedocnumber IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFUNCTION.
