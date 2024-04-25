class ZTMCL_DELIVERY_PROC definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .

  methods DETERMINA_COND_EXPEDICAO
    importing
      !IF_TRTYP type TRTYP
      !IF_TCODE type LE_SHP_TCODE
      !IT_XLIKP type SHP_LIKP_T
      !IT_YLIKP type SHP_YLIKP_T optional
      !IT_XLIPS type SHP_LIPS_T
      !IT_YLIPS type SHP_LIPS_T optional
      !IF_MODFRETE type J_1B_FREIGHT_MODE
      !IF_BSART_TM type XFELD
    changing
      !CS_LIPS type LIPS
      !CS_LIKP type LIKP .
protected section.
private section.
ENDCLASS.



CLASS ZTMCL_DELIVERY_PROC IMPLEMENTATION.


  METHOD determina_cond_expedicao.

    CHECK cs_likp-lfart  = 'EL'.

    CASE if_modfrete.
      WHEN '9'.
        cs_likp-vsbed       = 'Z3'.
      WHEN OTHERS.
    ENDCASE.

    cl_tms_int_cust=>get_tms_c_shp(
      EXPORTING
        iv_vstel     = cs_likp-vstel
        iv_lfart     = cs_likp-lfart
        iv_vsbed     = cs_likp-vsbed
      IMPORTING
        es_tms_c_shp = DATA(ls_tms_c_shp) ).

    cs_likp-tm_ctrl_key = ls_tms_c_shp-tm_ctrl_key.

  ENDMETHOD.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER.
  endmethod.


  METHOD if_ex_le_shp_delivery_proc~change_delivery_item.

    CHECK if_trtyp = 'V'  OR
          if_trtyp = 'H'.

    SELECT SINGLE bsart INTO @DATA(lv_bsart)
      FROM ekko
      WHERE ebeln = @cs_lips-vgbel.

    IF lv_bsart = 'ZPEP' OR lv_bsart = 'ZFUT'.
      DATA(lv_bsart_tm) = abap_true.
    ENDIF.

    SELECT SINGLE modfrete INTO @DATA(lv_modfrete)
      FROM j_1bmodfrete_det
      WHERE inco1    = @cs_likp-inco1.

    CALL METHOD me->determina_cond_expedicao
      EXPORTING
        if_trtyp      = if_trtyp
        if_tcode      = if_tcode
        if_modfrete   = lv_modfrete
        if_bsart_tm   = lv_bsart_tm
        it_xlikp      = it_xlikp
        it_xlips      = it_xlips
      CHANGING
        cs_lips       = cs_lips
        cs_likp       = cs_likp.


  ENDMETHOD.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT.
  endmethod.


  method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE.
  endmethod.
ENDCLASS.
